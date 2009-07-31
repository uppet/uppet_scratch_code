#include"prec.h"

#include<string>
#include<vector>
#include<iostream>
#include<sstream>
#include<fstream>
#include<cctype>
#include<map>
namespace epy{
    using std::vector;
    using std::string;
    using std::ifstream;
    using std::isalpha;
    using std::isalnum;
    using std::stringstream;
    using std::map;

    /**
       带引用管理handle对象
     */
    template<typename T, bool hold = true>
    class handle_rc {
        friend class handle_rc<T, !hold>;
        T * ob;
        int       * cnt;
        void dec_internal() {
            if (--(*cnt) < 1) {
                //std::cout<<"die"<<ob<<ob->to_string()<<std::endl;
                //std::cout<<ob->to_string();
                delete cnt;
                delete ob;
            }
        }
        void inc_internal() {
            *cnt += 1;
        }
    public:
        handle_rc(const handle_rc &other) {
            ob = other.ob;
            cnt = other.cnt;
            inc_internal();
        }
        handle_rc() {
            ob = NULL;
            if (hold)
                ob  = new T;
            cnt = new int(1);
        }
        T * operator->() {
            return ob;
        }
        T * it() {
            return ob;
        }
        template<typename T2>
        handle_rc& operator=(const T2  &other) {
            dec_internal();
            ob = other.ob;
            cnt = other.cnt;
            inc_internal();
            return *this;
        }
        handle_rc& operator=(const handle_rc  &other) {
            dec_internal();
            ob = other.ob;
            cnt = other.cnt;
            inc_internal();
            return *this;
        }
        ~handle_rc() {
            dec_internal();
        }
    };
    
    template<typename T, bool>
    class handle_rc;

    struct token_imp;
    struct tdparser_imp;
    struct scope_imp;

    typedef handle_rc<token_imp> token;
    typedef handle_rc<tdparser_imp> tdparser;
    typedef handle_rc<scope_imp> scope;
    typedef handle_rc<scope_imp, false> scopeh;
    
    enum token_type {
        OPERATOR,
        NAME,
        STRING,
        NUMBER
    };
    enum arity_type {
        ARITY_NAME,
        ARITY_STATEMENT,
        ARITY_UNARY,
        ARITY_BINARY,
        ARITY_LITERAL,
        ARITY_FUNCTION,
        ARITY_TERNARY,
        ARITY_THIS
    };

    struct nud_func{
        nud_func(){
            m_nud = NULL;
        }
        nud_func(token (*nud)(token , tdparser_imp *)) {
            m_nud = nud;
        }
        token operator()(token tok, tdparser_imp* prsr) {
            return do_call(tok, prsr);
        }
        virtual token do_call(token tok, tdparser_imp* prsr) {
            if (m_nud)
                return m_nud(tok, prsr);
        }
        token (*m_nud)(token , tdparser_imp *);
    };

    map<string, token> symbol_table;

    vector<token> get_tokens(const string &str);
    token         make_tok(token_type, const string &string_val, double double_val);

    struct token_imp {
        token_type type;
        string     string_val;
        double     double_val;
        int        int_val;
        string     id;
        bool       reserved;
        bool       isassign;
        token      (*nud)(token tok, tdparser_imp *prsr);
        token      (*led)(token tok, token left, tdparser_imp *prsr);
        token      (*std)(token tok, tdparser_imp *prsr);
        int        lbp;
        int        rbp;
        arity_type arity;
        scope      at_scope;
        int        pos_from;
        int        pos_to;
        vector<token> subs;
        void error(const string &err_msg) {
            std::cout<<err_msg<<to_string();
            exit(1);
        }
        string to_string() {
            stringstream ss;
            //ss<<"\ns: "<<string_val<<"\nd: "<<double_val<<std::endl;
            ss<<string_val<<std::endl;
            return ss.str();
        };
    };

    static scope s_cur_scope;
    struct scope_imp {
        map<string, token> m_def;
        scopeh   parent;
        
        token define (token tok);
        token find (string name);
        void pop ();
        void reserve (token tok);
        
        string to_string() {
            stringstream ss;
            return ss.str();
        };
    };

    struct tdparser_imp {
        vector<token> (*m_tokens_gen)(const string &);
        map<string, token> m_symbol_table;
        token m_cur_tok;
        size_t m_cur_idx;
        vector<token> m_tokens;
        token m_orig_symbol;
        
        scope m_cur_scope;
        scope new_scope();
        
        token advance(string id);
        token expression(int rbp);
        token statement();
        token statements();
        token block();
        token symbol(string id, int bp);
        token constant(string s, string s_val, double d_val);
        
        string to_string() {
            stringstream ss;
            ss<<this<<std::endl;
            return ss.str();
        }
        token do_parse(const string &str) {
            m_tokens = m_tokens_gen(str);
            m_cur_idx = 0;
            new_scope();
            advance("");
            token s = statements();
            advance("(end)");
            s_cur_scope->pop();
            return s;
        }
    };

    /**
       生成一个token
     */
    token make_token(token_type tt, const string &string_val, double double_val)
    {
        token tok;
        tok->type       = tt;
        tok->string_val = string_val;
        tok->double_val = double_val;
        return tok;
    }

    token copy_token(token &o)
    {
        token tok;
        //*tok.it() = *o.it();
        tok->type       = o->type;
        tok->string_val = o->string_val;
        tok->double_val = o->double_val;
        tok->int_val    = o->int_val;
        tok->id         = o->id;
        tok->reserved   = o->reserved;
        tok->isassign   = o->isassign;
        tok->nud        = o->nud;
        tok->led        = o->led;
        tok->std        = o->std;
        tok->lbp        = o->lbp;
        tok->rbp        = o->rbp;
        tok->arity      = o->arity;
        tok->at_scope   = o->at_scope;
        tok->pos_from   = o->pos_from;
        tok->pos_to     = o->pos_to;
        tok->subs       = o->subs;
//        tok->string_val = "1";
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

    token tok_undef(token tok, tdparser_imp *prsr) {
        tok->error("Undefined.");
        return token();
    }
    token tok_miss(token tok, token left, tdparser_imp *prsr) {
        tok->error("Missing operator.");
        return token();
    }
    token tok_itself(token tok, tdparser_imp *prsr) {
        return tok;
    }
    token tok_constant(token tok, tdparser_imp *prsr) {
        s_cur_scope->reserve(tok);
        tok->string_val = symbol_table[tok->id]->string_val;
        tok->double_val = symbol_table[tok->id]->double_val;
        tok->arity = ARITY_LITERAL;
        return tok;
    }
    

    token scope_imp::define(token tok)
    {
        if (m_def.count(tok->string_val)) {
            tok->error(m_def[tok->string_val]->reserved ? "Already reserved." : "Already defined.");
        }
        m_def[tok->string_val] = tok;
        tok->reserved = false;
        tok->nud      = tok_itself;
        tok->led      = NULL;
        tok->std      = NULL;
        tok->lbp      = 0;
        tok->at_scope = s_cur_scope;
        return tok;
    }

    token scope_imp::find(string name)
    {
        scope_imp *e = this; token o;
        for(;;) {
            if (e->m_def.count(name)) {
                return e->m_def[name];
            }
            e = e->parent.it();
            if (!e) {
                return symbol_table[ symbol_table.count(name) ?
                                     name : "(name)"];
            }
        }
    }
    
    void scope_imp::pop()
    {
            s_cur_scope = parent;
    }
    
    void scope_imp::reserve(token tok)
    {
        if (tok->arity != ARITY_NAME || tok->reserved) {
            return;
        }
        
        if (m_def.count(tok->string_val)) {
            token t = m_def[tok->string_val];
            if (t->reserved) {
                return;
            }
            if (t->arity == ARITY_NAME) {
                tok->error("Already defined.");
            }
        }
        m_def[tok->string_val] = tok;
        tok->reserved = true;
    }

    scope tdparser_imp::new_scope()
    {
        scope s = s_cur_scope;
        scope ns;
        ns->parent = s;
        s_cur_scope = ns;
        return ns;
    }

    token tdparser_imp::advance(string id)
    {
        if (!id.empty() && m_cur_tok->id != id) {
            m_cur_tok->error(string("Expected") + id + ".");
        }
        if (m_cur_idx >= m_tokens.size()) {
            m_cur_tok = symbol_table["(end)"];
            return m_cur_tok;
        }
        token t = m_tokens[m_cur_idx++];
        t->nud = NULL;
        t->led = NULL;
        t->std = NULL;
        token o;
        arity_type at = t->arity; //TODO grok from t->type
        string sv = t->string_val;
        token_type tv = t->type;
        if (tv == NAME) {
            o = m_cur_scope->find(sv);
        } else if (tv == OPERATOR) {
            if (!symbol_table.count(sv)) {
                t->error("Unknown operator.");
            }
            o = symbol_table[sv];
        } else if (tv == STRING || tv == NUMBER) {
            o  = symbol_table["(literal)"];
            at = ARITY_LITERAL;
        } else {
            t->error("Unknown token.");
        }
        token oo = copy_token(o);
        oo->pos_from = t->pos_from;
        oo->pos_to   = t->pos_to;
        oo->string_val = sv;
        oo->double_val = t->double_val;
        oo->arity      = at;
        return o;
    }

    token tdparser_imp::expression(int rbp)
    {
        token left;
        token t = m_cur_tok;
        advance("");
        left = t->nud(t, this);
        while (rbp < m_cur_tok->lbp) {
            t = m_cur_tok;
            advance("");
            left = t->led(t, left, this);
        }
        return left;
    }

    token tdparser_imp::statement()
    {
        token n = m_cur_tok;
        token v;
        if (n->std) {
            advance("");
            m_cur_scope->reserve(n);
            return n->std(n, this);
        }
        v = expression(0);
        if (!v->isassign && v->id != "(") {
            v->error("Bad expression statement.");
        }
        advance(";");
        return v;
    }

    token tdparser_imp::statements()
    {
        vector<token> a;
        token s;

        for(;;) {
            if (m_cur_tok->id == "}" || m_cur_tok->id == "(end)") {
                break;
            }
            s = statement();
            if (s->id != "") {
                a.push_back(s);
            }
        }
        token ta;
        ta->subs = a;
        return a.empty() ? token() : a.size() == 1 ? a[0] : ta;
    }

    token tdparser_imp::block()
    {
        token t = m_cur_tok;
        advance("{");
        return t->std(t, this);        
    }

    token tdparser_imp::symbol(string id, int bp)
    {
        token s;
        if (symbol_table.count(id)) {
            s = symbol_table[id];
            if (bp >= s->lbp)
                s->lbp = bp;
        } else {
            s = copy_token(m_orig_symbol);
            s->id = s->string_val = id;
            s->lbp = bp;
            symbol_table[id] = s;
        }
        return s;
    }

    token tdparser_imp::constant(string s, string s_val, double d_val)
    {
        token x = symbol(s, 0);
        x->nud = tok_constant;
        x->string_val = s_val;
        x->double_val = d_val;
        return x;
    }

    /**
       生成一个解析器对象
    */
    tdparser get_td_parser(vector<token> (*tokens_gen)(const string &)) {
        tdparser prsr;
        prsr->m_tokens_gen = tokens_gen;
        prsr->m_orig_symbol->nud = tok_undef;
        prsr->m_orig_symbol->led = tok_miss;
        prsr->m_orig_symbol->std = NULL;
        prsr->symbol("(end)", 0);
        prsr->symbol("(name)", 0);
        prsr->symbol(":", 0);
        prsr->symbol(";", 0);
        prsr->symbol(")", 0);
        prsr->symbol("]", 0);
        prsr->symbol("}", 0);
        prsr->symbol(",", 0);
        prsr->symbol("else", 0);
        return prsr;
    }
}

struct nn{
    nn(int (*nn_)()) {
        m_nn = nn_;
    }
    int operator()() {
        return do_call();
    }
    virtual int do_call(){
        if (m_nn)
            return m_nn();
        return 0;
    }
    int (*m_nn)();
};

struct nnt:nn{
    nnt(int init): nn(NULL) {
        m_init = init;
    }
    virtual int do_call(){
        return m_init + 1;
    }
    int m_init;
};

int one(){return 1;};

#include<sstream>
int main(int argc, char **argv)
{
    using namespace std;

    nn *  pn1 = new nn(one);
    vector<nn*> nns;
    for(int i=0; i<20; ++i) {
        nns.push_back(new nnt(i));
    }

    for(int i=0; i<20; ++i) {
        cout<<(*nns[i])()<<endl;
    }

    cout<<(*pn1)()<<endl;
    
    vector<epy::token> toks;
    string file = "tokens.js";
    if (argc > 2)
        file = argv[1];
//     toks = epy::get_tokens(epy::whole_file(file));
//     cout<<toks.size()<<" tokens in total"<<endl;
    epy::tdparser prsr = get_td_parser(epy::get_tokens);
    prsr->do_parse(epy::whole_file(file));

    return 0;
}
