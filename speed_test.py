#!/usr/bin/env python3
"""
局域网测速服务器 - Python实现（优化版）
功能：提供HTTP服务器，支持下载/上传测速
"""

import os
import sys
import random
import time
import threading
import socket
import argparse
from datetime import datetime
from http.server import HTTPServer, BaseHTTPRequestHandler
import json
import mimetypes
import struct
import math

# 预先生成的随机数据块（用于快速响应）
class RandomDataGenerator:
    """高效的随机数据生成器"""

    def __init__(self, chunk_size=1024*1024):  # 1MB块
        self.chunk_size = chunk_size
        self._cache = {}
        self._lock = threading.Lock()

    def get_chunk(self, size):
        """获取指定大小的随机数据"""
        if size <= 0:
            return b''

        # 尝试从缓存获取
        with self._lock:
            if size in self._cache:
                return self._cache[size]

        # 生成新数据
        if size <= 1024*1024:  # 小于1MB使用快速方法
            data = self._generate_fast(size)
        else:
            data = self._generate_large(size)

        # 缓存小尺寸数据
        if size <= 64*1024:  # 只缓存小于64KB的数据
            with self._lock:
                self._cache[size] = data

        return data

    def _generate_fast(self, size):
        """快速生成小尺寸随机数据"""
        # 使用os.urandom获取高质量随机数据
        return os.urandom(size)

    def _generate_large(self, size):
        """生成大尺寸随机数据"""
        # 对于大文件，使用重复模式减少内存分配
        chunk = 64*1024  # 64KB基础块
        repeats = size // chunk
        remainder = size % chunk

        # 生成一个基础块
        base_chunk = os.urandom(chunk)

        # 构建完整数据
        data = bytearray(size)
        for i in range(repeats):
            start = i * chunk
            end = start + chunk
            data[start:end] = base_chunk

        # 添加剩余部分
        if remainder:
            data[-remainder:] = os.urandom(remainder)

        return bytes(data)

# 全局随机数据生成器
random_generator = RandomDataGenerator()

class SpeedTestHandler(BaseHTTPRequestHandler):
    """HTTP请求处理器，提供测速功能"""

    # 禁用不必要的功能提高性能
    disable_nagle_algorithm = True  # 禁用Nagle算法
    timeout = 30  # 连接超时时间

    def setup(self):
        """设置连接参数"""
        super().setup()
        if self.disable_nagle_algorithm:
            # 禁用Nagle算法，减少延迟
            self.request.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)

    def do_GET(self):
        """处理GET请求"""
        # 路由处理
        if self.path == '/' or self.path == '/index.html':
            self.serve_index()
        elif self.path == '/ping':
            self.handle_ping()
        elif self.path.startswith('/download'):
            self.handle_download()
        elif self.path == '/stats':
            self.handle_stats()
        elif self.path.startswith('/api/'):
            self.handle_api()
        else:
            self.serve_static()

    def do_POST(self):
        """处理POST请求"""
        if self.path == '/upload':
            self.handle_upload()
        elif self.path == '/api/speedtest':
            self.handle_api_speedtest()
        else:
            self.send_error(404, "Not Found")

    def do_OPTIONS(self):
        """处理CORS预检请求"""
        self.send_response(200)
        self.send_cors_headers()
        self.end_headers()

    def send_cors_headers(self):
        """发送CORS头，允许跨域访问"""
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type')
        self.send_header('Cache-Control', 'no-cache, no-store, must-revalidate')
        self.send_header('Pragma', 'no-cache')
        self.send_header('Expires', '0')

    def serve_index(self):
        """提供测速页面（保持不变）"""
        html_content = """
<!DOCTYPE html>
<html lang="zh-CN">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>局域网测速工具 (Python版)</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; font-family: 'Segoe UI', Arial, sans-serif; }
        body { background: linear-gradient(135deg, #1a237e 0%, #311b92 100%); color: #fff; min-height: 100vh; padding: 20px; }
        .container { max-width: 900px; margin: 0 auto; padding: 20px; }
        header { text-align: center; margin-bottom: 30px; padding: 20px; background: rgba(255,255,255,0.1); border-radius: 15px; }
        h1 { font-size: 2.5rem; margin-bottom: 10px; color: #00bcd4; }
        .subtitle { font-size: 1.1rem; opacity: 0.8; }
        .server-info { background: rgba(255,255,255,0.08); padding: 15px; border-radius: 10px; margin-bottom: 20px; font-family: monospace; }
        .test-controls { display: flex; gap: 15px; margin-bottom: 30px; flex-wrap: wrap; justify-content: center; }
        .btn { padding: 12px 24px; border: none; border-radius: 50px; font-size: 1rem; cursor: pointer; transition: all 0.3s; }
        .btn-primary { background: linear-gradient(90deg, #2196f3, #21cbf3); color: white; }
        .btn-secondary { background: rgba(255,255,255,0.15); color: white; border: 2px solid rgba(255,255,255,0.3); }
        .btn:hover { transform: translateY(-3px); box-shadow: 0 5px 15px rgba(0,0,0,0.2); }
        .btn:disabled { opacity: 0.6; cursor: not-allowed; }
        .results { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px; margin-bottom: 25px; }
        .result-card { background: rgba(255,255,255,0.1); border-radius: 10px; padding: 20px; text-align: center; }
        .result-value { font-size: 2rem; font-weight: bold; margin: 10px 0; }
        .progress-container { height: 10px; background: rgba(255,255,255,0.1); border-radius: 5px; margin: 20px 0; overflow: hidden; }
        .progress-bar { height: 100%; background: linear-gradient(90deg, #00c853, #64dd17); width: 0%; transition: width 0.3s; }
        .status { text-align: center; margin: 15px 0; min-height: 24px; }
        .details { background: rgba(255,255,255,0.05); border-radius: 10px; padding: 15px; margin-top: 20px; }
        pre { background: rgba(0,0,0,0.3); padding: 10px; border-radius: 5px; overflow-x: auto; }
        .history { margin-top: 20px; }
        .history-item { background: rgba(255,255,255,0.08); padding: 10px; border-radius: 8px; margin-bottom: 8px; display: flex; justify-content: space-between; }
        footer { text-align: center; margin-top: 30px; padding-top: 20px; border-top: 1px solid rgba(255,255,255,0.1); opacity: 0.7; }
        @media (max-width: 600px) {
            .container { padding: 10px; }
            h1 { font-size: 1.8rem; }
            .test-controls { flex-direction: column; }
            .btn { width: 100%; }
            .results { grid-template-columns: 1fr; }
        }
    </style>
</head>
<body>
    <div class="container">
        <header>
            <h1>局域网测速工具</h1>
            <p class="subtitle">Python HTTP服务器版</p>
        </header>
        
        <div class="server-info">
            <p>服务器地址: <span id="server-address">正在获取...</span></p>
            <p>服务器状态: <span id="server-status">运行中</span></p>
        </div>
        
        <div class="test-controls">
            <button id="start-test" class="btn btn-primary">开始测速</button>
            <button id="download-test" class="btn btn-secondary">测试下载</button>
            <button id="upload-test" class="btn btn-secondary">测试上传</button>
        </div>
        
        <div class="progress-container">
            <div class="progress-bar" id="progress-bar"></div>
        </div>
        
        <div class="status" id="status">准备开始测速...</div>
        
        <div class="results">
            <div class="result-card">
                <h3>下载速度</h3>
                <div class="result-value" id="download-speed">--</div>
                <div>Mbps</div>
            </div>
            <div class="result-card">
                <h3>上传速度</h3>
                <div class="result-value" id="upload-speed">--</div>
                <div>Mbps</div>
            </div>
            <div class="result-card">
                <h3>延迟</h3>
                <div class="result-value" id="latency">--</div>
                <div>ms</div>
            </div>
        </div>
        
        <div class="details">
            <h3>测试详情</h3>
            <pre id="test-details">等待测试开始...</pre>
        </div>
        
        <div class="history">
            <h3>历史记录</h3>
            <div id="history-list"></div>
        </div>
        
        <footer>
            <p>局域网测速工具 &copy; 2023 - 基于Python HTTP服务器</p>
        </footer>
    </div>
    
    <script>
        // 获取DOM元素
        const startBtn = document.getElementById('start-test');
        const downloadBtn = document.getElementById('download-test');
        const uploadBtn = document.getElementById('upload-test');
        const downloadSpeedEl = document.getElementById('download-speed');
        const uploadSpeedEl = document.getElementById('upload-speed');
        const latencyEl = document.getElementById('latency');
        const statusEl = document.getElementById('status');
        const progressBar = document.getElementById('progress-bar');
        const testDetailsEl = document.getElementById('test-details');
        const historyListEl = document.getElementById('history-list');
        const serverAddressEl = document.getElementById('server-address');
        
        // 更新服务器地址
        serverAddressEl.textContent = window.location.origin;
        
        // 测速配置
        const CONFIG = {
            downloadSize: 5 * 1024 * 1024, // 5MB
            uploadSize: 2 * 1024 * 1024,   // 2MB
            pingCount: 5,
            testDuration: 5000
        };
        
        // 更新状态和进度
        function updateStatus(msg, progress) {
            statusEl.textContent = msg;
            progressBar.style.width = progress + '%';
        }
        
        // 测试延迟
        async function testLatency() {
            updateStatus('测试延迟...', 10);
            let total = 0;
            let success = 0;
            
            for (let i = 0; i < CONFIG.pingCount; i++) {
                const start = performance.now();
                try {
                    const response = await fetch('/ping', {cache: 'no-store'});
                    if (response.ok) {
                        const end = performance.now();
                        total += (end - start);
                        success++;
                        updateStatus(`延迟测试 ${i+1}/${CONFIG.pingCount}`, 10 + (i/CONFIG.pingCount)*20);
                    }
                } catch (e) {
                    console.warn('Ping失败:', e);
                }
                await new Promise(r => setTimeout(r, 100));
            }
            
            if (success > 0) {
                const avg = total / success;
                latencyEl.textContent = avg.toFixed(1);
                return avg;
            }
            return null;
        }
        
        // 测试下载速度
        async function testDownload() {
            updateStatus('测试下载速度...', 30);
            const start = performance.now();
            
            try {
                const response = await fetch(`/download?size=${CONFIG.downloadSize}`, {cache: 'no-store'});
                if (!response.ok) throw new Error(`HTTP ${response.status}`);
                
                const reader = response.body.getReader();
                let received = 0;
                
                while (true) {
                    const {done, value} = await reader.read();
                    if (done) break;
                    received += value.length;
                    
                    const elapsed = performance.now() - start;
                    const progress = 30 + Math.min(40, (elapsed / CONFIG.testDuration) * 40);
                    updateStatus(`下载: ${(received/1024/1024).toFixed(1)} MB`, progress);
                }
                
                const time = (performance.now() - start) / 1000;
                const speed = (received * 8 / time / 1000000).toFixed(2);
                downloadSpeedEl.textContent = speed;
                updateStatus('下载测试完成', 70);
                
                return {speed, bytes: received, time};
            } catch (e) {
                console.error('下载测试失败:', e);
                updateStatus('下载测试失败', 30);
                return null;
            }
        }
        
        // 测试上传速度
        async function testUpload() {
            updateStatus('准备上传数据...', 70);
            
            // 生成测试数据
            const data = new Uint8Array(CONFIG.uploadSize);
            for (let i = 0; i < data.length; i++) {
                data[i] = Math.floor(Math.random() * 256);
            }
            
            updateStatus('测试上传速度...', 80);
            const start = performance.now();
            
            try {
                const response = await fetch('/upload', {
                    method: 'POST',
                    headers: {'Content-Type': 'application/octet-stream'},
                    body: data
                });
                
                if (!response.ok) throw new Error(`HTTP ${response.status}`);
                
                const time = (performance.now() - start) / 1000;
                const speed = (CONFIG.uploadSize * 8 / time / 1000000).toFixed(2);
                uploadSpeedEl.textContent = speed;
                updateStatus('上传测试完成', 100);
                
                return {speed, bytes: CONFIG.uploadSize, time};
            } catch (e) {
                console.error('上传测试失败:', e);
                updateStatus('上传测试失败', 70);
                return null;
            }
        }
        
        // 完整测速
        async function runFullTest() {
            // 重置显示
            downloadSpeedEl.textContent = '--';
            uploadSpeedEl.textContent = '--';
            latencyEl.textContent = '--';
            testDetailsEl.textContent = '测试开始...\\n';
            
            // 禁用按钮
            startBtn.disabled = downloadBtn.disabled = uploadBtn.disabled = true;
            
            try {
                // 测试延迟
                const latency = await testLatency();
                
                // 测试下载
                const download = await testDownload();
                
                // 测试上传
                const upload = await testUpload();
                
                // 更新详情
                let details = `测速时间: ${new Date().toLocaleString()}\\n`;
                if (latency) details += `延迟: ${latency.toFixed(1)} ms\\n`;
                if (download) details += `下载: ${download.speed} Mbps (${(download.bytes/1024/1024).toFixed(2)} MB in ${download.time.toFixed(2)}s)\\n`;
                if (upload) details += `上传: ${upload.speed} Mbps (${(upload.bytes/1024/1024).toFixed(2)} MB in ${upload.time.toFixed(2)}s)`;
                testDetailsEl.textContent = details;
                
                // 添加到历史记录
                addToHistory(latency, download, upload);
                
                updateStatus('测速完成!', 100);
            } catch (e) {
                console.error('测速失败:', e);
                updateStatus('测速失败: ' + e.message, 0);
                testDetailsEl.textContent = '测速失败: ' + e.message;
            } finally {
                // 启用按钮
                startBtn.disabled = downloadBtn.disabled = uploadBtn.disabled = false;
                
                // 重置进度条
                setTimeout(() => {
                    progressBar.style.width = '0%';
                    statusEl.textContent = '准备开始新的测速...';
                }, 3000);
            }
        }
        
        // 仅下载测试
        async function runDownloadTest() {
            downloadSpeedEl.textContent = '--';
            uploadSpeedEl.textContent = '--';
            latencyEl.textContent = '--';
            testDetailsEl.textContent = '下载测试开始...\\n';
            
            startBtn.disabled = downloadBtn.disabled = uploadBtn.disabled = true;
            
            try {
                const latency = await testLatency();
                const download = await testDownload();
                
                let details = `下载测试时间: ${new Date().toLocaleString()}\\n`;
                if (latency) details += `延迟: ${latency.toFixed(1)} ms\\n`;
                if (download) details += `下载: ${download.speed} Mbps (${(download.bytes/1024/1024).toFixed(2)} MB in ${download.time.toFixed(2)}s)`;
                testDetailsEl.textContent = details;
                
                addToHistory(latency, download, null);
                
                updateStatus('下载测试完成!', 100);
            } catch (e) {
                console.error('下载测试失败:', e);
                updateStatus('下载测试失败: ' + e.message, 0);
                testDetailsEl.textContent = '下载测试失败: ' + e.message;
            } finally {
                startBtn.disabled = downloadBtn.disabled = uploadBtn.disabled = false;
                setTimeout(() => {
                    progressBar.style.width = '0%';
                    statusEl.textContent = '准备开始新的测试...';
                }, 3000);
            }
        }
        
        // 仅上传测试
        async function runUploadTest() {
            downloadSpeedEl.textContent = '--';
            uploadSpeedEl.textContent = '--';
            latencyEl.textContent = '--';
            testDetailsEl.textContent = '上传测试开始...\\n';
            
            startBtn.disabled = downloadBtn.disabled = uploadBtn.disabled = true;
            
            try {
                const latency = await testLatency();
                const upload = await testUpload();
                
                let details = `上传测试时间: ${new Date().toLocaleString()}\\n`;
                if (latency) details += `延迟: ${latency.toFixed(1)} ms\\n`;
                if (upload) details += `上传: ${upload.speed} Mbps (${(upload.bytes/1024/1024).toFixed(2)} MB in ${upload.time.toFixed(2)}s)`;
                testDetailsEl.textContent = details;
                
                addToHistory(latency, null, upload);
                
                updateStatus('上传测试完成!', 100);
            } catch (e) {
                console.error('上传测试失败:', e);
                updateStatus('上传测试失败: ' + e.message, 0);
                testDetailsEl.textContent = '上传测试失败: ' + e.message;
            } finally {
                startBtn.disabled = downloadBtn.disabled = uploadBtn.disabled = false;
                setTimeout(() => {
                    progressBar.style.width = '0%';
                    statusEl.textContent = '准备开始新的测试...';
                }, 3000);
            }
        }
        
        // 添加到历史记录
        function addToHistory(latency, download, upload) {
            const item = document.createElement('div');
            item.className = 'history-item';
            
            const time = new Date().toLocaleTimeString();
            let text = `${time} - `;
            if (latency) text += `延迟:${latency.toFixed(1)}ms `;
            if (download) text += `下载:${download.speed}Mbps `;
            if (upload) text += `上传:${upload.speed}Mbps`;
            
            item.textContent = text;
            historyListEl.prepend(item);
            
            // 限制记录数量
            while (historyListEl.children.length > 10) {
                historyListEl.removeChild(historyListEl.lastChild);
            }
            
            // 保存到本地存储
            try {
                const history = JSON.parse(localStorage.getItem('speedtest-history') || '[]');
                history.unshift({
                    time: new Date().toISOString(),
                    latency: latency,
                    download: download ? download.speed : null,
                    upload: upload ? upload.speed : null
                });
                if (history.length > 10) history.length = 10;
                localStorage.setItem('speedtest-history', JSON.stringify(history));
            } catch (e) {
                console.warn('无法保存历史记录:', e);
            }
        }
        
        // 加载历史记录
        function loadHistory() {
            try {
                const history = JSON.parse(localStorage.getItem('speedtest-history') || '[]');
                history.forEach(item => {
                    const elem = document.createElement('div');
                    elem.className = 'history-item';
                    
                    const time = new Date(item.time).toLocaleTimeString();
                    let text = `${time} - `;
                    if (item.latency) text += `延迟:${item.latency.toFixed(1)}ms `;
                    if (item.download) text += `下载:${item.download}Mbps `;
                    if (item.upload) text += `上传:${item.upload}Mbps`;
                    
                    elem.textContent = text;
                    historyListEl.appendChild(elem);
                });
            } catch (e) {
                console.warn('无法加载历史记录:', e);
            }
        }
        
        // 事件监听
        startBtn.addEventListener('click', runFullTest);
        downloadBtn.addEventListener('click', runDownloadTest);
        uploadBtn.addEventListener('click', runUploadTest);
        
        // 加载历史记录
        document.addEventListener('DOMContentLoaded', loadHistory);
    </script>
</body>
</html>
"""
        
        self.send_response(200)
        self.send_header('Content-Type', 'text/html; charset=utf-8')
        self.send_cors_headers()
        self.end_headers()
        self.wfile.write(html_content.encode('utf-8'))

        # ... (保持原有HTML代码不变)

    def serve_static(self):
        """提供静态文件（如果需要的话）"""
        self.send_error(404, "File Not Found")

    def handle_ping(self):
        """处理延迟测试请求"""
        self.send_response(200)
        self.send_header('Content-Type', 'text/plain')
        self.send_cors_headers()
        self.end_headers()
        self.wfile.write(b'pong')

    def handle_download(self):
        """处理下载测速请求（优化版）"""
        # 获取请求的大小参数
        import urllib.parse
        query = urllib.parse.urlparse(self.path).query
        params = urllib.parse.parse_qs(query)

        # 默认5MB，最大100MB
        size = 5 * 1024 * 1024  # 5MB
        if 'size' in params:
            try:
                req_size = int(params['size'][0])
                size = min(req_size, 100 * 1024 * 1024)  # 限制最大100MB
            except:
                pass

        # 使用优化的数据块大小
        # 根据总大小调整块大小
        if size <= 1024*1024:  # 小于1MB
            chunk_size = 64*1024  # 64KB
        elif size <= 10*1024*1024:  # 小于10MB
            chunk_size = 256*1024  # 256KB
        else:  # 大于10MB
            chunk_size = 1024*1024  # 1MB

        self.send_response(200)
        self.send_header('Content-Type', 'application/octet-stream')
        self.send_header('Content-Length', str(size))
        self.send_cors_headers()
        self.end_headers()

        # 优化：使用缓冲区减少系统调用
        remaining = size
        buffer_size = 0
        max_buffer = 4 * 1024 * 1024  # 最大4MB缓冲区

        while remaining > 0:
            # 计算本次发送的块大小
            chunk = min(chunk_size, remaining)

            # 获取随机数据
            data = random_generator.get_chunk(chunk)

            # 发送数据
            self.wfile.write(data)
            remaining -= chunk

            # 定期刷新缓冲区
            buffer_size += chunk
            if buffer_size >= max_buffer:
                self.wfile.flush()
                buffer_size = 0

        # 确保所有数据都已发送
        self.wfile.flush()

    def handle_upload(self):
        """处理上传测速请求（优化版）"""
        # 获取内容长度
        content_length = int(self.headers.get('Content-Length', 0))

        # 优化：使用更大的缓冲区读取数据
        chunk_size = 64 * 1024  # 64KB
        received = 0

        # 直接丢弃数据，不存储到内存
        while received < content_length:
            chunk = min(chunk_size, content_length - received)
            data = self.rfile.read(chunk)
            if not data:
                break
            received += len(data)

        # 返回结果
        result = {
            'received': received,
            'timestamp': datetime.now().isoformat(),
            'status': 'success'
        }

        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_cors_headers()
        self.end_headers()
        self.wfile.write(json.dumps(result).encode('utf-8'))

    def handle_stats(self):
        """处理服务器状态请求"""
        stats = {
            'server': 'Python SpeedTest Server (Optimized)',
            'version': '2.0',
            'uptime': self.server.get_uptime(),
            'requests': self.server.request_count,
            'timestamp': datetime.now().isoformat()
        }

        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_cors_headers()
        self.end_headers()
        self.wfile.write(json.dumps(stats, indent=2).encode('utf-8'))

    def handle_api(self):
        """处理API请求"""
        if self.path == '/api/status':
            stats = {
                'status': 'running',
                'timestamp': datetime.now().isoformat()
            }
            self.send_json_response(stats)
        else:
            self.send_error(404, "API endpoint not found")

    def handle_api_speedtest(self):
        """处理API测速请求"""
        content_length = int(self.headers.get('Content-Length', 0))
        if content_length:
            body = self.rfile.read(content_length)
            data = json.loads(body.decode('utf-8'))
            test_type = data.get('type', 'full')

            result = {
                'type': test_type,
                'timestamp': datetime.now().isoformat(),
                'status': 'accepted'
            }
            self.send_json_response(result)
        else:
            self.send_error(400, "Bad Request")

    def send_json_response(self, data):
        """发送JSON响应"""
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_cors_headers()
        self.end_headers()
        self.wfile.write(json.dumps(data).encode('utf-8'))

    def log_message(self, format, *args):
        """自定义日志格式"""
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        print(f"[{timestamp}] {self.address_string()} - {format % args}")

class SpeedTestServer(HTTPServer):
    """自定义HTTP服务器，添加额外功能"""

    def __init__(self, server_address, RequestHandlerClass):
        super().__init__(server_address, RequestHandlerClass)
        self.start_time = time.time()
        self.request_count = 0

    def get_uptime(self):
        """获取服务器运行时间"""
        uptime = time.time() - self.start_time
        if uptime < 60:
            return f"{int(uptime)}秒"
        elif uptime < 3600:
            return f"{int(uptime/60)}分钟"
        else:
            return f"{int(uptime/3600)}小时{int((uptime%3600)/60)}分钟"

def get_local_ip():
    """获取本机局域网IP地址"""
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        s.connect(("8.8.8.8", 80))
        ip = s.getsockname()[0]
        s.close()
        return ip
    except:
        return "127.0.0.1"

def get_all_network_ips():
    """获取所有网络接口的IP地址"""
    ips = []
    try:
        import netifaces
        for interface in netifaces.interfaces():
            addrs = netifaces.ifaddresses(interface)
            if netifaces.AF_INET in addrs:
                for addr_info in addrs[netifaces.AF_INET]:
                    ip = addr_info['addr']
                    if ip != '127.0.0.1' and not ip.startswith('169.254.'):
                        ips.append(ip)
    except ImportError:
        ips.append(get_local_ip())

    return ips

def main():
    """主函数"""
    parser = argparse.ArgumentParser(description='局域网测速服务器（优化版）')
    parser.add_argument('--port', '-p', type=int, default=8000, help='服务器端口 (默认: 8000)')
    parser.add_argument('--host', '-H', default='0.0.0.0', help='服务器地址 (默认: 0.0.0.0 - 所有接口)')
    parser.add_argument('--threads', '-t', type=int, default=10, help='线程池大小 (默认: 10)')

    args = parser.parse_args()

    # 创建服务器
    server_address = (args.host, args.port)
    httpd = SpeedTestServer(server_address, SpeedTestHandler)

    # 设置socket选项优化性能
    httpd.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    httpd.socket.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)

    # 获取可访问的地址
    local_ip = get_local_ip()
    all_ips = get_all_network_ips()

    print("=" * 60)
    print("局域网测速服务器（优化版）已启动!")
    print("=" * 60)
    print(f"服务器运行在: {args.host}:{args.port}")
    print(f"\n本地访问:")
    print(f"  http://localhost:{args.port}")
    print(f"  http://127.0.0.1:{args.port}")

    print(f"\n局域网访问:")
    for ip in all_ips:
        print(f"  http://{ip}:{args.port}")

    print(f"\n优化特性:")
    print(f"  - 使用os.urandom()生成随机数据")
    print(f"  - 智能数据块大小调整")
    print(f"  - 禁用Nagle算法减少延迟")
    print(f"  - 缓冲区优化减少系统调用")
    print("=" * 60)
    print("按 Ctrl+C 停止服务器")

    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        print("\n正在关闭服务器...")
        httpd.shutdown()
        print("服务器已关闭")

if __name__ == '__main__':
    main()
