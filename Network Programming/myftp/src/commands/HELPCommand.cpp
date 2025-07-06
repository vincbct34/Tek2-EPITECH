/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** HELPCommand.cpp
*/

#include "HELPCommand.hpp"

std::string HELPCommand::execute(const std::string& args, ClientSession& session) {
    (void)args;
    (void)session;

    std::string response = "214 The following commands are recognized:\r\n"
                           "USER PASS CWD CDUP QUIT DELE PWD PASV PORT HELP NOOP RETR STOR LIST\r\n";

    return response;
}
