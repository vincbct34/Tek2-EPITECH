/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** RETRCommand.hpp
*/

#include "RETRCommand.hpp"
#include "SocketActions.hpp"

std::string RETRCommand::execute(const std::string& args, ClientSession& session) {
    if (args.empty())
        return "501 Syntax error in parameters.\r\n";

    std::string trimmedArgs = args;
    trimmedArgs.erase(trimmedArgs.find_last_not_of(" \n\r\t") + 1);

    if (std::filesystem::is_directory(trimmedArgs))
        return "550 Cannot RETR a directory.\r\n";

    std::ifstream file(trimmedArgs, std::ios::binary);

    if (!file.is_open())
        return "550 File not found or cannot be opened.\r\n";

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

        while (!file.eof()) {
            file.read(buffer, sizeof(buffer));
            SocketActions::writeToSocket(dataSocket, buffer, file.gcount());
        }

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
