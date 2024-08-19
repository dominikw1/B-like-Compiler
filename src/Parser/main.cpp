#include "Parser.h"
#include "Scanner.h"
#include <iostream>
#include <span>

constexpr auto websiteProgram = R"(  
    gauss(x) {
        register res = -0;
        while (x > 0) {
            res = res + x;
            x = x - 1;
        }
        return res;
    }

    ifTest(x) {
        if (x < -5)
            return;
        x = x + 3;
    }

    isBool(x) { return !!x == x; }

    baz(a) { return; }



    foo(a, b) {
        a[b] = b;
        return a[b] + a[b@1];
    }
    callTest(a, b) {
        register c = foo(a, b);
        return baz(c) + baz(a) + baz(c);
    }

    unreachableCode(a) {
        if (a > 0) return a;
        else return -a;
        return a + 1;
    }


    addrof(ptr) {
        auto var = 1;
        ptr[1] = &var;
        register ptr2 = &ptr[1];
        ptr2[0] = 2;
        return var;
    }
)";

int main() {

    try {
        auto lexed{scan(websiteProgram)};
        auto ast{parse(lexed)};
    } catch (std::exception& e) {
        std::cout << "exception " << e.what() << std::endl;
    }
}