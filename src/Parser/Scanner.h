#pragma once
#include "Token.h"
#include <string_view>
#include <vector>

std::vector<Token> scan(std::string_view program);