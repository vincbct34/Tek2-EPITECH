/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** QUITCommand.cpp
*/

#include "QUITCommand.hpp"

std::string QUITCommand::execute(const std::string& args, ClientSession& session) {
    (void)args;
    SocketActions::closeSocket(session.clientSocket);
    return "221 Service closing control connection.\r\n";
}
