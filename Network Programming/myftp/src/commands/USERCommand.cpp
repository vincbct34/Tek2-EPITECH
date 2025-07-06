/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** USERCommand.cpp
*/

#include "USERCommand.hpp"

std::string USERCommand::execute(const std::string& args, ClientSession& session) {
    (void)args;

    if (args == "Anonymous\r\n") {
        session.username = "Anonymous";
    }

    return "331 User name okay, need password.\r\n";
}
