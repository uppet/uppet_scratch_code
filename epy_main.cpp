
#include<string>
#include<vector>
#include<iostream>
#include<fstream>
namespace epy{
    using std::vector;
    using std::string;
    using std::ifstream;

    class token;
    struct token_imp;
    enum token_type;
    vector<token> get_tokens(const string &str);
    token         make_tok(token_type, const string &string_val, double double_val);

    enum token_type {
        OPERATOR,
        NAME,
        STRING,
        NUMBER
    };

    struct token_imp {
        string string_val;
        double double_val;
        int    int_val;
        int    pos_from;
        int    pos_to;
    };

    /**
       带引用管理token对象
     */
    class token {
        token_imp * ob;
        int       * cnt;
        void dec_internal() {
            if (--(*cnt) < 1) {
                std::cout<<"die"<<ob<<ob->string_val<<std::endl;
                delete cnt;
                delete ob;
            }
        }
        void inc_internal() {
            *cnt += 1;
        }
    public:
        token(const token &other) {
            ob = other.ob;
            cnt = other.cnt;
            inc_internal();
        }
        token() {
            ob  = new token_imp;
            cnt = new int(1);
        }
        token_imp * operator->() {
            return ob;
        }
        token& operator=(const token &other) {
            dec_internal();
            ob = other.ob;
            cnt = other.cnt;
            inc_internal();
            return *this;
        }
        ~token() {
            dec_internal();
        }
    };

    /**
       生成一个token
     */
    token make_token(token_type, const string &string_val, double double_val)
    {
        token tok;
        tok->string_val = string_val;
        tok->double_val = double_val;
        return tok;
    }

    /**
       将字符串解析为token表
     */
    vector<token> get_tokens(const string &str)
    {
        vector<token> tokens;
        size_t i = 0;
        int from = 0;
        
        if (str.size())
            char c = str[i];
        while (i < str.size()) {
            from = i;
        }
    
        return tokens;
    }

    /**
       获取整个文件的内容到一个string当中
     */
    string whole_file(const string &file) {
        string line;
        ifstream fin(file.c_str());
        vector<string> lines;
        int len = 0;
        while(getline(fin, line)) {
            lines.push_back(line);
            len += line.size() + 1;
        }
        string whole;
        whole.reserve(len);
        for(size_t i=0; i<lines.size();++i){
            whole.append(lines[i]);
            whole+="\n";
        }
        return whole;
    }
}


#include<sstream>
int main(int argc, char **argv)
{
    using namespace std;
    vector<epy::token> toks;
    string file = "h:\\check\\epvm\\fe\\epy_main.cpp";
    if (argc > 2)
        file = argv[1];
    cout<<epy::whole_file(file);


    return 0;
}
