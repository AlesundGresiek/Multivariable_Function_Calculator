#include <SFML/Graphics.hpp>

#include <algorithm>
#include <cctype>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

using namespace std;

// ============================================================
// EXPRESSION TREE
// ============================================================

struct Node
{
    string op;
    double value;
    shared_ptr<Node> left, right;

    Node(string operation, double number = 0,
         shared_ptr<Node> lhs = nullptr,
         shared_ptr<Node> rhs = nullptr)
        : op(std::move(operation)), value(number),
          left(std::move(lhs)), right(std::move(rhs)) {}
};

using P = shared_ptr<Node>;

P num(double value)
{
    return make_shared<Node>("num", value);
}

P variable(const string& name)
{
    return make_shared<Node>(name);
}

P unary(const string& op, P a)
{
    if (op == "neg")
    {
        if (a->op == "num")
            return num(-a->value);
        if (a->op == "neg")
            return a->left;
    }

    if (op == "ln" && a->op == "e")
        return num(1);

    return make_shared<Node>(op, 0, a);
}

P bin(const string& op, P a, P b)
{
    if (a->op == "num" && b->op == "num")
    {
        const double x = a->value;
        const double y = b->value;

        if (op == "+") return num(x + y);
        if (op == "-") return num(x - y);
        if (op == "*") return num(x * y);
        if (op == "/" && y != 0.0) return num(x / y);

        if (op == "^")
        {
            const double result = pow(x, y);
            if (isfinite(result))
                return num(result);
        }
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
        if (a->op == "num" && a->value == -1) return unary("neg", b);
        if (b->op == "num" && b->value == -1) return unary("neg", a);
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

bool zero(const P& a)
{
    return a->op == "num" && a->value == 0;
}

// ============================================================
// EXPRESSION PARSER
// Supported: x, y, z, e, pi, + - * / ^, parentheses,
// sin, cos, tan, ln, log10, exp.
// Also supports implicit multiplication such as 3x, x(y+1), xy, xyz.
// ============================================================

class Parser
{
    string s;
    size_t pos = 0;

    void spaces()
    {
        while (pos < s.size() && isspace(static_cast<unsigned char>(s[pos])))
            ++pos;
    }

    bool startsAtom()
    {
        spaces();
        if (pos >= s.size()) return false;
        const char c = s[pos];
        return isalpha(static_cast<unsigned char>(c)) ||
               isdigit(static_cast<unsigned char>(c)) || c == '(' || c == '.';
    }

    P expression()
    {
        P a = term();
        while (true)
        {
            spaces();
            if (pos < s.size() && s[pos] == '+')
            {
                ++pos;
                a = bin("+", a, term());
            }
            else if (pos < s.size() && s[pos] == '-')
            {
                ++pos;
                a = bin("-", a, term());
            }
            else
            {
                break;
            }
        }
        return a;
    }

    P term()
    {
        P a = unaryExpr();
        while (true)
        {
            spaces();
            if (pos < s.size() && s[pos] == '*')
            {
                ++pos;
                a = bin("*", a, unaryExpr());
            }
            else if (pos < s.size() && s[pos] == '/')
            {
                ++pos;
                a = bin("/", a, unaryExpr());
            }
            else if (startsAtom())
            {
                // Implicit multiplication: 3x, 2(x+1), sin(x)cos(y), etc.
                a = bin("*", a, unaryExpr());
            }
            else
            {
                break;
            }
        }
        return a;
    }

    P unaryExpr()
    {
        spaces();
        if (pos < s.size() && s[pos] == '+')
        {
            ++pos;
            return unaryExpr();
        }
        if (pos < s.size() && s[pos] == '-')
        {
            ++pos;
            return unary("neg", unaryExpr());
        }
        return power();
    }

    P power()
    {
        P a = primary();
        spaces();
        if (pos < s.size() && s[pos] == '^')
        {
            ++pos;
            a = bin("^", a, unaryExpr());
        }
        return a;
    }

    P primary()
    {
        spaces();
        if (pos >= s.size())
            throw string("Unexpected end of expression.");

        if (s[pos] == '(')
        {
            ++pos;
            P a = expression();
            spaces();
            if (pos >= s.size() || s[pos] != ')')
                throw string("Missing closing parenthesis.");
            ++pos;
            return a;
        }

        if (isdigit(static_cast<unsigned char>(s[pos])) || s[pos] == '.')
        {
            const size_t start = pos;
            bool seenDot = false;
            bool seenDigit = false;

            while (pos < s.size())
            {
                const char c = s[pos];
                if (isdigit(static_cast<unsigned char>(c)))
                {
                    seenDigit = true;
                    ++pos;
                }
                else if (c == '.' && !seenDot)
                {
                    seenDot = true;
                    ++pos;
                }
                else
                {
                    break;
                }
            }

            if (!seenDigit)
                throw string("Invalid number. Enter a number such as 2, 0.5, or 3.14.");

            // If another decimal point follows immediately, reject the whole token
            // instead of accidentally parsing it as implicit multiplication.
            if (pos < s.size() && s[pos] == '.')
                throw string("Invalid number: too many decimal points.");

            const string token = s.substr(start, pos - start);
            size_t charactersUsed = 0;
            const double value = stod(token, &charactersUsed);
            if (charactersUsed != token.size())
                throw string("Invalid number: ") + token;
            return num(value);
        }

        if (isalpha(static_cast<unsigned char>(s[pos])))
        {
            const size_t start = pos;
            while (pos < s.size() && isalnum(static_cast<unsigned char>(s[pos])))
                ++pos;

            const string name = s.substr(start, pos - start);

            if (name == "x" || name == "y" || name == "z")
                return variable(name);
            if (name == "e")
                return variable("e");
            if (name == "pi")
                return num(acos(-1.0));

            if (name == "sin" || name == "cos" || name == "tan" ||
                name == "ln" || name == "log10" || name == "exp")
            {
                spaces();
                if (pos >= s.size() || s[pos] != '(')
                    throw string("Function '") + name + "' needs parentheses, e.g. " + name + "(x).";
                ++pos;
                P a = expression();
                spaces();
                if (pos >= s.size() || s[pos] != ')')
                    throw string("Missing closing parenthesis after ") + name + "(...).";
                ++pos;
                return unary(name, a);
            }

            // Treat adjacent x/y/z letters as multiplied variables: xy means x*y.
            bool onlyVariables = !name.empty();
            for (char c : name)
            {
                if (c != 'x' && c != 'y' && c != 'z')
                {
                    onlyVariables = false;
                    break;
                }
            }
            if (onlyVariables)
            {
                P result = variable(string(1, name[0]));
                for (size_t i = 1; i < name.size(); ++i)
                    result = bin("*", result, variable(string(1, name[i])));
                return result;
            }

            throw string("Unknown name: ") + name;
        }

        throw string("Invalid character in expression.");
    }

public:
    explicit Parser(string input) : s(std::move(input)) {}

    P parse()
    {
        P result = expression();
        spaces();
        if (pos != s.size())
            throw string("Could not parse the complete expression. Check the operators and parentheses.");
        return result;
    }
};

// ============================================================
// SYMBOLIC DIFFERENTIATION
// ============================================================

P derivative(P a, const string& v)
{
    const string op = a->op;

    if (op == "num" || op == "e")
        return num(0);

    if (op == "x" || op == "y" || op == "z")
        return num(op == v ? 1 : 0);

    P u = a->left;
    P w = a->right;

    if (op == "neg")
        return unary("neg", derivative(u, v));

    P du = derivative(u, v);
    P dw = (w ? derivative(w, v) : num(0));

    if (op == "+")
        return bin("+", du, dw);

    if (op == "-")
        return bin("-", du, dw);

    if (op == "*")
        return bin("+", bin("*", du, w), bin("*", u, dw));

    if (op == "/")
        return bin("/",
                   bin("-", bin("*", du, w), bin("*", u, dw)),
                   bin("^", w, num(2)));

    if (op == "^")
    {
        // Constant exponent: d(u^c) = c*u^(c-1)*u'
        if (zero(dw))
            return bin("*", bin("*", w, bin("^", u, bin("-", w, num(1)))), du);

        // Constant base: d(c^w) = c^w * ln(c) * w'
        if (zero(du))
            return bin("*", bin("*", a, unary("ln", u)), dw);

        // Variable base and variable exponent: d(u^w) = u^w * (w' ln(u) + w*u'/u)
        return bin("*", a,
                   bin("+", bin("*", dw, unary("ln", u)),
                       bin("*", w, bin("/", du, u))));
    }

    if (op == "sin")
        return bin("*", unary("cos", u), du);

    if (op == "cos")
        return bin("*", unary("neg", unary("sin", u)), du);

    if (op == "tan")
        return bin("*", bin("/", num(1), bin("^", unary("cos", u), num(2))), du);

    if (op == "ln")
        return bin("/", du, u);

    if (op == "log10")
        return bin("/", du, bin("*", u, num(log(10.0))));

    if (op == "exp")
        return bin("*", unary("exp", u), du);

    // Fail loudly instead of silently returning a mathematically wrong zero.
    throw string("Differentiation is not implemented for operation: ") + op;
}

// ============================================================
// EXPRESSION PRINTER
// ============================================================

string print(P a)
{
    const string op = a->op;

    if (op == "num")
    {
        ostringstream out;
        out << setprecision(6) << defaultfloat << a->value;
        return out.str();
    }

    if (op == "x" || op == "y" || op == "z" || op == "e")
        return op;

    if (op == "neg")
        return "- (" + print(a->left) + ")";

    if (op == "sin" || op == "cos" || op == "tan" ||
        op == "ln" || op == "log10" || op == "exp")
        return op + "(" + print(a->left) + ")";

    return "(" + print(a->left) + " " + op + " " + print(a->right) + ")";
}

// ============================================================
// GRAPHICAL INTERFACE HELPERS
// ============================================================

enum class ButtonAction
{
    Insert,
    Backspace,
    Clear,
    Solve
};

struct Button
{
    string label;
    string insertedText;
    float x;
    float y;
    float width;
    float height;
    ButtonAction action;
};

void drawText(sf::RenderWindow& window, const sf::Font& font,
              const string& content, unsigned int size,
              float x, float y, sf::Color color = sf::Color(235, 240, 250))
{
    sf::Text text(font, content, size);
    text.setPosition({x, y});
    text.setFillColor(color);
    window.draw(text);
}

void addButton(vector<Button>& buttons, const string& label,
               const string& insertedText, float x, float y,
               float width, float height, ButtonAction action)
{
    buttons.push_back({label, insertedText, x, y, width, height, action});
}

vector<Button> createButtons()
{
    vector<Button> buttons;
    const float startX = 40.f;
    const float startY = 170.f;
    const float buttonW = 124.f;
    const float buttonH = 48.f;
    const float gap = 8.f;

    const vector<vector<string>> keys = {
        {"x", "y", "z", "7", "8", "9", "("},
        {")", "^", "*", "/", "4", "5", "6"},
        {"sin(", "cos(", "tan(", "ln(", "1", "2", "3"},
        {"log10(", "exp(", "e", "pi", "0", ".", "-"}
    };

    for (size_t row = 0; row < keys.size(); ++row)
    {
        for (size_t col = 0; col < keys[row].size(); ++col)
        {
            const string key = keys[row][col];
            addButton(buttons, key, key,
                      startX + static_cast<float>(col) * (buttonW + gap),
                      startY + static_cast<float>(row) * (buttonH + gap),
                      buttonW, buttonH, ButtonAction::Insert);
        }
    }

    // Wide action row below the mathematical keys.
    const float actionY = startY + 4.f * (buttonH + gap);
    const float actionH = 48.f;
    const float actionGap = 8.f;
    const float actionW = (920.f - 4.f * actionGap) / 5.f;

    addButton(buttons, "+", "+", 40.f, actionY, actionW, actionH, ButtonAction::Insert);
    addButton(buttons, "=", "", 40.f + (actionW + actionGap), actionY,
              actionW, actionH, ButtonAction::Solve);
    addButton(buttons, "Backspace", "", 40.f + 2.f * (actionW + actionGap), actionY,
              actionW, actionH, ButtonAction::Backspace);
    addButton(buttons, "Clear", "", 40.f + 3.f * (actionW + actionGap), actionY,
              actionW, actionH, ButtonAction::Clear);
    addButton(buttons, "Differentiate", "", 40.f + 4.f * (actionW + actionGap), actionY,
              actionW, actionH, ButtonAction::Solve);

    return buttons;
}

bool pointInside(const Button& button, float x, float y)
{
    return x >= button.x && x <= button.x + button.width &&
           y >= button.y && y <= button.y + button.height;
}

void drawButton(sf::RenderWindow& window, const sf::Font& font,
                const Button& button, float mouseX, float mouseY)
{
    const bool hovered = pointInside(button, mouseX, mouseY);
    sf::RectangleShape shape({button.width, button.height});
    shape.setPosition({button.x, button.y});
    shape.setOutlineThickness(1.f);
    shape.setOutlineColor(sf::Color(74, 91, 119));

    if (button.action == ButtonAction::Solve)
        shape.setFillColor(hovered ? sf::Color(47, 115, 192) : sf::Color(35, 91, 158));
    else if (button.action == ButtonAction::Clear || button.action == ButtonAction::Backspace)
        shape.setFillColor(hovered ? sf::Color(131, 70, 73) : sf::Color(92, 51, 57));
    else
        shape.setFillColor(hovered ? sf::Color(61, 76, 100) : sf::Color(39, 50, 69));

    window.draw(shape);

    const unsigned int fontSize = button.label.size() > 9 ? 15U : 18U;
    const float estimatedTextWidth = static_cast<float>(button.label.size()) *
                                     static_cast<float>(fontSize) * 0.54f;
    const float textX = button.x + (button.width - estimatedTextWidth) / 2.f;
    const float textY = button.y + (button.height - static_cast<float>(fontSize)) / 2.f - 2.f;
    drawText(window, font, button.label, fontSize, textX, textY);
}

vector<string> wrapText(const string& text, size_t maxCharacters)
{
    vector<string> lines;
    if (text.empty())
    {
        lines.emplace_back("");
        return lines;
    }

    size_t start = 0;
    while (start < text.size())
    {
        if (text.size() - start <= maxCharacters)
        {
            lines.push_back(text.substr(start));
            break;
        }

        size_t split = text.rfind(' ', start + maxCharacters);
        if (split == string::npos || split <= start)
            split = start + maxCharacters;

        lines.push_back(text.substr(start, split - start));
        start = split;
        while (start < text.size() && text[start] == ' ')
            ++start;
    }
    return lines;
}

void appendWrapped(vector<string>& destination, const string& text)
{
    vector<string> wrapped = wrapText(text, 112);
    destination.insert(destination.end(), wrapped.begin(), wrapped.end());
}

void solveExpression(string input, vector<string>& resultLines)
{
    resultLines.clear();

    // Permit either an expression alone or a definition such as f(x,y,z)=...
    const size_t equalPos = input.find('=');
    if (equalPos != string::npos)
        input = input.substr(equalPos + 1);

    if (input.find_first_not_of(" \t\r\n") == string::npos)
    {
        resultLines.push_back("Enter a function first, using the keypad or keyboard.");
        return;
    }

    try
    {
        Parser parser(input);
        P f = parser.parse();

        P dx = derivative(f, "x");
        P dy = derivative(f, "y");
        P dz = derivative(f, "z");

        resultLines.push_back("FUNCTION");
        appendWrapped(resultLines, "f(x, y, z) = " + print(f));

        resultLines.push_back("");
        resultLines.push_back("PARTIAL DERIVATIVE WITH RESPECT TO X");
        appendWrapped(resultLines, "df/dx = " + print(dx));

        resultLines.push_back("");
        resultLines.push_back("PARTIAL DERIVATIVE WITH RESPECT TO Y");
        appendWrapped(resultLines, "df/dy = " + print(dy));

        resultLines.push_back("");
        resultLines.push_back("PARTIAL DERIVATIVE WITH RESPECT TO Z");
        appendWrapped(resultLines, "df/dz = " + print(dz));

        if (resultLines.size() > 14)
        {
            resultLines.resize(13);
            resultLines.push_back("... Output shortened to fit this panel.");
        }
    }
    catch (const string& error)
    {
        resultLines.push_back("Input error:");
        appendWrapped(resultLines, error);
    }
    catch (const exception& error)
    {
        resultLines.push_back("Could not process this expression:");
        appendWrapped(resultLines, error.what());
    }
    catch (...)
    {
        resultLines.push_back("An unexpected error occurred while parsing the function.");
    }
}

// ============================================================
// MAIN: SFML 3 GRAPHICAL CALCULATOR
// ============================================================

int main()
{
    sf::RenderWindow window(
        sf::VideoMode({1000U, 850U}),
        "Symbolic Partial Derivative Calculator");
    window.setFramerateLimit(60);

    sf::Font font;
    // This path exists on a typical Windows installation.
    // If it fails, change it to the path of another .ttf font on your PC.
    if (!font.openFromFile("C:/Windows/Fonts/arial.ttf"))
    {
        cerr << "Could not load C:/Windows/Fonts/arial.ttf\n"
             << "Edit the font path in main() to a valid .ttf font file.\n";
        return 1;
    }

    string input;
    vector<string> resultLines = {
        "Click the expression field and use the buttons to build a function.",
        "Press Differentiate to calculate df/dx, df/dy, and df/dz.",
        "Example: x^2*y + y*z^3"
    };

    const vector<Button> buttons = createButtons();
    const sf::FloatRect inputBounds({40.f, 105.f}, {920.f, 46.f});
    bool inputFocused = true;

    while (window.isOpen())
    {
        while (const optional event = window.pollEvent())
        {
            if (event->is<sf::Event::Closed>())
            {
                window.close();
            }
            else if (const auto* keyPressed = event->getIf<sf::Event::KeyPressed>())
            {
                if (keyPressed->code == sf::Keyboard::Key::Escape)
                    window.close();
                else if (inputFocused && keyPressed->code == sf::Keyboard::Key::Backspace)
                {
                    if (!input.empty()) input.pop_back();
                }
                else if (inputFocused && keyPressed->code == sf::Keyboard::Key::Enter)
                {
                    solveExpression(input, resultLines);
                }
            }
            else if (const auto* textEntered = event->getIf<sf::Event::TextEntered>())
            {
                if (inputFocused && textEntered->unicode >= 32 && textEntered->unicode < 127)
                {
                    const char c = static_cast<char>(textEntered->unicode);
                    if (isalnum(static_cast<unsigned char>(c)) ||
                        string("+-*/^().= ").find(c) != string::npos)
                    {
                        input += c;
                    }
                }
            }
            else if (const auto* mousePressed = event->getIf<sf::Event::MouseButtonPressed>())
            {
                if (mousePressed->button != sf::Mouse::Button::Left)
                    continue;

                const float mouseX = static_cast<float>(mousePressed->position.x);
                const float mouseY = static_cast<float>(mousePressed->position.y);

                if (inputBounds.contains({mouseX, mouseY}))
                {
                    inputFocused = true;
                    continue;
                }

                for (const Button& button : buttons)
                {
                    if (!pointInside(button, mouseX, mouseY))
                        continue;

                    inputFocused = true;
                    if (button.action == ButtonAction::Insert)
                    {
                        input += button.insertedText;
                    }
                    else if (button.action == ButtonAction::Backspace)
                    {
                        if (!input.empty()) input.pop_back();
                    }
                    else if (button.action == ButtonAction::Clear)
                    {
                        input.clear();
                        resultLines.clear();
                    }
                    else if (button.action == ButtonAction::Solve)
                    {
                        solveExpression(input, resultLines);
                    }
                    break;
                }
            }
        }

        const sf::Vector2i mousePosition = sf::Mouse::getPosition(window);
        const float mouseX = static_cast<float>(mousePosition.x);
        const float mouseY = static_cast<float>(mousePosition.y);

        window.clear(sf::Color(17, 22, 33));

        drawText(window, font, "PARTIAL DERIVATIVE CALCULATOR", 25, 40.f, 18.f,
                 sf::Color(120, 190, 255));
        drawText(window, font, "Enter f(x,y,z); use the mouse keypad or type with your keyboard.",
                 15, 40.f, 55.f, sf::Color(173, 184, 202));
        drawText(window, font, "FUNCTION", 14, 40.f, 82.f, sf::Color(173, 184, 202));

        sf::RectangleShape inputBox({920.f, 46.f});
        inputBox.setPosition({40.f, 105.f});
        inputBox.setFillColor(sf::Color(27, 35, 50));
        inputBox.setOutlineThickness(1.5f);
        inputBox.setOutlineColor(inputFocused ? sf::Color(85, 164, 238)
                                              : sf::Color(74, 91, 119));
        window.draw(inputBox);

        string visibleInput = input;
        const size_t maxInputChars = 100;
        if (visibleInput.size() > maxInputChars)
            visibleInput = "..." + visibleInput.substr(visibleInput.size() - maxInputChars + 3);
        drawText(window, font, visibleInput + (inputFocused ? "|" : ""), 20, 52.f, 115.f);

        for (const Button& button : buttons)
            drawButton(window, font, button, mouseX, mouseY);

        drawText(window, font, "RESULTS", 18, 40.f, 450.f,
                 sf::Color(120, 190, 255));
        sf::RectangleShape resultBox({920.f, 330.f});
        resultBox.setPosition({40.f, 478.f});
        resultBox.setFillColor(sf::Color(23, 30, 43));
        resultBox.setOutlineThickness(1.f);
        resultBox.setOutlineColor(sf::Color(58, 71, 94));
        window.draw(resultBox);

        float lineY = 493.f;
        const size_t maxLinesToDraw = 14;
        size_t linesDrawn = 0;
        for (const string& line : resultLines)
        {
            if (linesDrawn >= maxLinesToDraw) break;
            sf::Color color = sf::Color(224, 231, 242);
            if (line.rfind("Input error", 0) == 0 ||
                line.rfind("Could not", 0) == 0 ||
                line.rfind("An unexpected", 0) == 0)
                color = sf::Color(255, 151, 151);

            drawText(window, font, line, 15, 53.f, lineY, color);
            lineY += 21.f;
            ++linesDrawn;
        }

        drawText(window, font,
                 "Supported: +  -  *  /  ^  sin  cos  tan  ln  log10  exp  pi  e",
                 12, 40.f, 820.f, sf::Color(144, 156, 176));

        window.display();
    }

    return 0;
}