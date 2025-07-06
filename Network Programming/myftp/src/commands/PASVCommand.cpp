/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** PASVCommand.cpp
*/

#include "ClientSession.hpp"
#include "SocketActions.hpp"
#include "PASVCommand.hpp"

std::string PASVCommand::execute(const std::string& args, ClientSession& session) {
    (void)args;

    int pasvSocket = SocketActions::openSocket(AF_INET, SOCK_STREAM, 0);

    if (pasvSocket == -1) {
        return "425 Can't open passive connection.\r\n";
    }

    sockaddr_in pasvAddr{};
    pasvAddr.sin_family = AF_INET;
    pasvAddr.sin_addr.s_addr = INADDR_ANY;
    pasvAddr.sin_port = 0;

    if (SocketActions::bindSocket(pasvSocket, (struct sockaddr*)&pasvAddr, sizeof(pasvAddr)) == -1) {
        SocketActions::closeSocket(pasvSocket);
        return "425 Can't bind passive connection.\r\n";
    }

    socklen_t addrLen = sizeof(pasvAddr);

    if (SocketActions::getSocketName(pasvSocket, (struct sockaddr*)&pasvAddr, &addrLen) == -1) {
        SocketActions::closeSocket(pasvSocket);
        return "425 Error getting passive port.\r\n";
    }

    int port = SocketActions::getPort(pasvAddr.sin_port);

    if (SocketActions::listenSocket(pasvSocket, 1) == -1) {
        SocketActions::closeSocket(pasvSocket);
        return "425 Can't listen on passive port.\r\n";
    }

    session.pasvSocket = pasvSocket;

    std::ostringstream response;

    response << "227 Entering Passive Mode (127,0,0,1," << (port / 256) << "," << (port % 256) << ").\r\n";

    return response.str();
}
