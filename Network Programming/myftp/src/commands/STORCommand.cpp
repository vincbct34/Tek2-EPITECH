/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** STORCommand.cpp
*/

#include "STORCommand.hpp"
#include "SocketActions.hpp"

std::string STORCommand::execute(const std::string& args, ClientSession& session) {
    if (args.empty())
        return "501 Syntax error in parameters.\r\n";

    std::string filename = args;

    filename.erase(filename.find_last_not_of(" \n\r\t") + 1);
    std::ofstream file(filename, std::ios::binary);

    if (!file.is_open())
        return "550 Cannot create file.\r\n";

    int dataSocket = session.dataSocket > 0 ? session.dataSocket : SocketActions::acceptConnection(session.pasvSocket, nullptr, nullptr);

    if (dataSocket < 0) {
        file.close();
        return "425 No data connection.\r\n";
    }

    pid_t pid = SocketActions::forkProcess();

    if (pid < 0) {
        file.close();
        SocketActions::closeSocket(dataSocket);
        return "550 Failed to start file transfer.\r\n";
    }

    if (pid == 0) {
        if (SocketActions::writeToSocket(session.clientSocket, "150 Opening BINARY mode data connection.\r\n", 42) < 0)
            SocketActions::exitProcess(1);

        char buffer[4096];
        ssize_t bytesRead;

        while ((bytesRead = SocketActions::readFromSocket(dataSocket, buffer, sizeof(buffer))) > 0)
            file.write(buffer, bytesRead);

        file.close();
        SocketActions::writeToSocket(session.clientSocket, "226 File transfer successful.\r\n", 31);
        SocketActions::closeSocket(dataSocket);
        SocketActions::exitProcess(0);
    }

    file.close();
    SocketActions::closeSocket(dataSocket);
    session.dataSocket = -1;

    return "";
}
