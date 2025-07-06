/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** PASSCommand.cpp
*/

#include "PASSCommand.hpp"

std::string PASSCommand::execute(const std::string& args, ClientSession& session) {
    if (session.username == "Anonymous") {
        if (args == "\r\n" || args == "PASS\r\n") {
            session.isAuthenticated = true;
            return "230 User logged in, proceed.\r\n";
        }
    }
    return "530 Not logged in.\r\n";
}
