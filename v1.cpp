#include <iostream>
#include <string>
#include <memory>
#include <cctype>
#include <cmath>
#include <sstream>
#include <iomanip>
using namespace std;

struct Node
{
    string op;
    double value;
    shared_ptr<Node> left, right;

    Node(string o, double v = 0,
         shared_ptr<Node> l = nullptr,
         shared_ptr<Node> r = nullptr)
        : op(o), value(v), left(l), right(r) {}
};

using P = shared_ptr<Node>;

P num(double v)
{
    return make_shared<Node>("num", v);
}

P variable(string s)
{
    return make_shared<Node>(s);
}

P unary(string op, P a)
{
    if (op == "neg")
    {
        if (a->op == "num") return num(-a->value);
        if (a->op == "neg") return a->left;
    }

    if (op == "ln" && a->op == "e") return num(1);

    return make_shared<Node>(op, 0, a);
}

P bin(string op, P a, P b)
{
    if (a->op == "num" && b->op == "num")
    {
        double x = a->value, y = b->value;

        if (op == "+") return num(x + y);
        if (op == "-") return num(x - y);
        if (op == "*") return num(x * y);
        if (op == "/" && y != 0) return num(x / y);
        if (op == "^") return num(pow(x, y));
    }

    if (op == "+")
    {
        if (a->op == "num" && a->value == 0) return b;
        if (b->op == "num" && b->value == 0) return a;
    }

    if (op == "-")
    {
        if (b->op == "num" && b->value == 0) return a;
        if (a->op == "num" && a->value == 0)
            return unary("neg", b);
    }

    if (op == "*")
    {
        if ((a->op == "num" && a->value == 0) ||
            (b->op == "num" && b->value == 0))
            return num(0);

        if (a->op == "num" && a->value == 1) return b;
        if (b->op == "num" && b->value == 1) return a;

        if (a->op == "num" && a->value == -1)
            return unary("neg", b);

        if (b->op == "num" && b->value == -1)
            return unary("neg", a);
    }

    if (op == "/")
    {
        if (a->op == "num" && a->value == 0) return num(0);
        if (b->op == "num" && b->value == 1) return a;
    }

    if (op == "^")
    {
        if (b->op == "num" && b->value == 0) return num(1);
        if (b->op == "num" && b->value == 1) return a;
        if (a->op == "num" && a->value == 1) return num(1);
    }

    return make_shared<Node>(op, 0, a, b);
}

bool zero(P a)
{
    return a->op == "num" && a->value == 0;
}

// ---------------- EXPRESSION PARSER ----------------

class Parser
{
    string s;
    int pos = 0;

    void spaces()
    {
        while (pos < (int)s.size() &&
               isspace((unsigned char)s[pos]))
            pos++;
    }

    bool startsAtom()
    {
        spaces();

        if (pos >= (int)s.size()) return false;

        char c = s[pos];
        return isalpha((unsigned char)c) ||
               isdigit((unsigned char)c) || c == '(';
    }

    P expression()
    {
        P a = term();

        while (true)
        {
            spaces();

            if (pos < (int)s.size() && s[pos] == '+')
            {
                pos++;
                a = bin("+", a, term());
            }
            else if (pos < (int)s.size() && s[pos] == '-')
            {
                pos++;
                a = bin("-", a, term());
            }
            else break;
        }

        return a;
    }

    P term()
    {
        P a = unaryExpr();

        while (true)
        {
            spaces();

            if (pos < (int)s.size() && s[pos] == '*')
            {
                pos++;
                a = bin("*", a, unaryExpr());
            }
            else if (pos < (int)s.size() && s[pos] == '/')
            {
                pos++;
                a = bin("/", a, unaryExpr());
            }
            else if (startsAtom())
            {
                // Supports implicit multiplication, e.g. xy and 3y
                a = bin("*", a, unaryExpr());
            }
            else break;
        }

        return a;
    }

    P unaryExpr()
    {
        spaces();

        if (pos < (int)s.size() && s[pos] == '+')
        {
            pos++;
            return unaryExpr();
        }

        if (pos < (int)s.size() && s[pos] == '-')
        {
            pos++;
            return unary("neg", unaryExpr());
        }

        return power();
    }

    P power()
    {
        P a = primary();
        spaces();

        if (pos < (int)s.size() && s[pos] == '^')
        {
            pos++;
            a = bin("^", a, unaryExpr());
        }

        return a;
    }

    P primary()
    {
        spaces();

        if (pos >= (int)s.size())
            throw string("Unexpected end of expression.");

        if (s[pos] == '(')
        {
            pos++;
            P a = expression();
            spaces();

            if (pos >= (int)s.size() || s[pos] != ')')
                throw string("Missing closing parenthesis.");

            pos++;
            return a;
        }

        if (isdigit((unsigned char)s[pos]) || s[pos] == '.')
        {
            size_t start = pos;

            while (pos < (int)s.size() &&
                   (isdigit((unsigned char)s[pos]) ||
                    s[pos] == '.'))
                pos++;

            return num(stod(s.substr(start, pos - start)));
        }

        if (isalpha((unsigned char)s[pos]))
        {
            size_t start = pos;

            while (pos < (int)s.size() &&
                   isalnum((unsigned char)s[pos]))
                pos++;

            string name = s.substr(start, pos - start);

            if (name == "x" || name == "y")
                return variable(name);

            if (name == "e")
                return variable("e");

            if (name == "pi")
                return num(acos(-1.0));

            if (name == "sin" || name == "cos" ||
                name == "tan" || name == "ln" ||
                name == "log10" || name == "exp")
            {
                spaces();

                if (pos >= (int)s.size() || s[pos] != '(')
                    throw string("Function needs parentheses.");

                pos++;
                P a = expression();
                spaces();

                if (pos >= (int)s.size() || s[pos] != ')')
                    throw string("Missing closing parenthesis.");

                pos++;
                return unary(name, a);
            }

            throw string("Unknown name: ") + name;
        }

        throw string("Invalid character in expression.");
    }

public:
    Parser(string input) : s(input) {}

    P parse()
    {
        P result = expression();
        spaces();

        if (pos != (int)s.size())
            throw string("Could not parse the complete expression.");

        return result;
    }
};

// ---------------- SYMBOLIC DIFFERENTIATION ----------------

P derivative(P a, string v)
{
    string op = a->op;

    if (op == "num" || op == "e")
        return num(0);

    if (op == "x" || op == "y")
        return num(op == v ? 1 : 0);

    P u = a->left;
    P w = a->right;

    if (op == "neg")
        return unary("neg", derivative(u, v));

    P du = derivative(u, v);

    if (op == "+")
        return bin("+", du, derivative(w, v));

    if (op == "-")
        return bin("-", du, derivative(w, v));

    if (op == "*")
        return bin("+",
                   bin("*", du, w),
                   bin("*", u, derivative(w, v)));

    if (op == "/")
        return bin("/",
                   bin("-", bin("*", du, w),
                            bin("*", u, derivative(w, v))),
                   bin("^", w, num(2)));

    if (op == "^")
    {
        P dw = derivative(w, v);

        // Constant exponent: power rule
        if (zero(dw))
        {
            return bin("*",
                       bin("*", w,
                           bin("^", u,
                               bin("-", w, num(1)))),
                       du);
        }

        // Constant base: exponential rule
        if (zero(du))
        {
            return bin("*",
                       bin("*", a, unary("ln", u)),
                       dw);
        }

        // Variable base and variable exponent: x^y rule
        return bin("*", a,
                   bin("+",
                       bin("*", dw, unary("ln", u)),
                       bin("*", w, bin("/", du, u))));
    }

    if (op == "sin")
        return bin("*", unary("cos", u), du);

    if (op == "cos")
        return bin("*",
                   unary("neg", unary("sin", u)), du);

    if (op == "tan")
        return bin("*",
                   bin("/", num(1),
                       bin("^", unary("cos", u), num(2))),
                   du);

    if (op == "ln")
        return bin("/", du, u);

    if (op == "log10")
        return bin("/", du,
                   bin("*", u, num(log(10.0))));

    if (op == "exp")
        return bin("*", unary("exp", u), du);

    return num(0);
}

// ---------------- PRINT THE ANSWER ----------------

string print(P a)
{
    string op = a->op;

    if (op == "num")
    {
        ostringstream out;
        out << setprecision(6) << defaultfloat << a->value;
        return out.str();
    }

    if (op == "x" || op == "y" || op == "e")
        return op;

    if (op == "neg")
        return "-(" + print(a->left) + ")";

    if (op == "sin" || op == "cos" || op == "tan" ||
        op == "ln" || op == "log10" || op == "exp")
        return op + "(" + print(a->left) + ")";

    return "(" + print(a->left) + " " + op + " " +
           print(a->right) + ")";
}

int main()
{
    string input;
    int choice;

    do
    {
        cout << "\n=== AUTOMATIC PARTIAL DERIVATIVE CALCULATOR ===\n";
        cout << "Enter f(x,y), or type exit to quit.\n";
        cout << "Example: x^2*y^3 + e^(xy) + ln(x^2+y^2+1)\n";
        cout << "Function: ";

        getline(cin, input);

        if (input == "exit" || input == "EXIT")
            break;

        try
        {
            // Accept input such as f(x,y) = x^2 + y^3
            size_t equalPos = input.find('=');

            if (equalPos != string::npos)
                input = input.substr(equalPos + 1);

            Parser parser(input);
            P f = parser.parse();

            P dx = derivative(f, "x");
            P dy = derivative(f, "y");

            cout << "\nf(x,y) = " << print(f) << endl;
            cout << "Partial derivative with respect to x:\n";
            cout << "df/dx = " << print(dx) << endl;

            cout << "\nPartial derivative with respect to y:\n";
            cout << "df/dy = " << print(dy) << endl;
        }
        catch (const string &error)
        {
            cout << "Error: " << error << endl;
        }
        catch (...)
        {
            cout << "Invalid expression. Please try again.\n";
        }

        cout << "\nEnter 1 to solve another function or 0 to exit: ";
        if (!(cin >> choice))
            break;

        cin.ignore(10000, '\n');

    } while (choice != 0);

    cout << "\nCalculator closed.\n";
    return 0;
}
