
#include<string>
#include<vector>
#include<iostream>
#include<sstream>
#include<fstream>
#include<cctype>
namespace epy{
    using std::vector;
    using std::string;
    using std::ifstream;
    using std::isalpha;
    using std::isalnum;
    using std::stringstream;

    class token;
    struct token_imp;

    enum token_type {
        OPERATOR,
        NAME,
        STRING,
        NUMBER
    };
    
    vector<token> get_tokens(const string &str);
    token         make_tok(token_type, const string &string_val, double double_val);

    struct token_imp {
        string string_val;
        double double_val;
        int    int_val;
        int    pos_from;
        int    pos_to;
        void error(const string &err_msg) {
            std::cout<<err_msg<<to_string();
        }
        string to_string() {
            stringstream ss;
            ss<<"\ns: "<<string_val<<"\nd: "<<double_val<<std::endl;
            return ss.str();
        };
    };

    /**
       带引用管理token对象
     */
    class token {
        token_imp * ob;
        int       * cnt;
        void dec_internal() {
            if (--(*cnt) < 1) {
                //std::cout<<"die"<<ob<<ob->to_string()<<std::endl;
                std::cout<<ob->string_val<<std::endl;
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

    char charat(const string &str, size_t i) {
        if (i < str.size())
            return str[i];
        return 0;
    }
    
    /**
       将字符串解析为token表
     */
    vector<token> get_tokens(const string &str)
    {
        vector<token> tokens;
        size_t i = 0;
        int from = 0;

        string prefix = "<>+-&";
        string suffix = "=>&:";
        
        char c = charat(str, i);
        while (c) {
            from = i;
            string str_val;
            
            if ( c <= ' ') {
                i++;
                c = charat(str, i);
            } else if (isalpha(c)) {
                str_val += c;
                i++;
                for(;;) {
                    c = charat(str, i);
                    if (isalnum(c) || c == '_') {
                        str_val += c;
                        i++;
                        continue;
                    }
                    break;
                }
                tokens.push_back(make_token(NAME, str_val, 0));
            } else if (c >= '0' && c <= '9' ) {
                str_val += c;
                i++;
                for(;;) {
                    c = charat(str, i);
                    if (c < '0' || c > '9')
                        break;
                    i++;
                    str_val += c;
                }
                if (c == '.') {
                    i++;
                    str_val += c;
                    for (;;) {
                        c = charat(str, i);
                        if (c < '0' || c > '9') {
                            break;
                        }
                        i += 1;
                        str_val += c;
                    }
                }
                if (c == 'e' || c == 'E') {
                    i += 1;
                    str_val += c;
                    c = charat(str, i);
                    if (c == '-' || c == '+') {
                        i += 1;
                        str_val += c;
                        c = charat(str, i);
                    }
                    if (c < '0' || c > '9') {
                        make_token(NUMBER, str_val, strtod(str_val.c_str(), 0))->error("Bad exponent");
                    }
                    do {
                        i += 1;
                        str_val += c;
                        c = charat(str, i);
                    } while (c >= '0' && c <= '9');
                }
                if (isalpha(c)) {
                    str_val += c;
                    i += 1;
                    make_token(NUMBER, str_val, strtod(str_val.c_str(), 0))->error("Bad number");
                }

                tokens.push_back(make_token(NUMBER, str_val, strtod(str_val.c_str(), 0)));
            } else if (c == '\'' || c == '"') {
                str_val = "";
                char q = c;
                i++;
                for(;;) {
                    c = charat(str, i);
                    if (c < ' ') {
                        make_token(STRING, str_val, 0)->error( c == '\n' || c == '\r' || c == 0 ?
                                                               "Unterminated string." :
                                                               "Control character in string."); //TODO append make('',str_val)
                    }

                    if (c == q) {
                        break;
                    }

                    if (c == '\\') {
                        i++;
                        if (i >= str.size()) {
                            make_token(STRING, str_val, 0)->error("Unterminated string.");
                        }
                        c = charat(str, i);
                        switch(c) {
                        case 'b':
                            c = '\b';
                            break;
                        case 'f':
                            c = '\f';
                            break;
                        case 'n':
                            c = '\n';
                            break;
                        case 'r':
                            c = '\r';
                            break;
                        case 't':
                            c = '\t';
                            break;
                        case 'u':
                            if (i >= str.size()) {
                                make_token(STRING, str_val, 0)->error("Unterminated string.");
                            }
                            c = atoi(str.substr(i + 1, 4).c_str());
//                             if (!isFinite(c) || c < 0) {
//                                 make('string', str).error("Unterminated string");
//                             }
                            i += 4;
                            break;                            
                        }
                    }
                    str_val += c;
                    i++;
                }
                i++;
                tokens.push_back(make_token(STRING, str_val, 0));
                c = charat(str, i);
            } else if (c == '/' && charat(str, i + 1) == '/') {
                i++;
                for (;;) {
                    c = charat(str, i);
                    if (c == '\n' || c == '\r' || c == 0) {
                        break;
                    }
                    i += 1;
                }
            } else if (prefix.find(c) != string::npos) {
                str_val = c;
                i++;
                while (i < str.size()) {
                    c = charat(str, i);
                    if (suffix.find(c) == string::npos) {
                        break;
                    }
                    str_val += c;
                    i++;
                }
                tokens.push_back(make_token(OPERATOR, str_val, 0));

            } else {
                i++;
                str_val += c;
                tokens.push_back(make_token(OPERATOR, str_val, 0));
                c = charat(str, i);
            }
//             else {  /// Final un reached;
//                 i++;
//                 c = charat(str, i);
//             }
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
    string file = "tokens.js";
    if (argc > 2)
        file = argv[1];
    toks = epy::get_tokens(epy::whole_file(file));
    cout<<toks.size()<<" tokens in total"<<endl;


    return 0;
}
