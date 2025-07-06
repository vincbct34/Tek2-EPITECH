/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** PWDCommand.cpp
*/

#include "PWDCommand.hpp"

std::string PWDCommand::execute(const std::string& args, ClientSession& session) {
    (void)args;
    (void)session;
    char cwd[PATH_MAX];

    if (getcwd(cwd, sizeof(cwd)) != nullptr) {
        return "257 \"" + std::string(cwd) + "\"\r\n";
    }

    return "550 Failed to get current directory.\r\n";
}
