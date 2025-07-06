/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** LISTCommand.cpp
*/

#include "ClientSession.hpp"
#include "SocketActions.hpp"
#include "LISTCommand.hpp"

std::string LISTCommand::execute(const std::string& args, ClientSession& session) {
    int dataSocket = session.dataSocket > 0 ? session.dataSocket : SocketActions::acceptConnection(session.pasvSocket, nullptr, nullptr);

    if (dataSocket < 0)
        return "425 No data connection.\r\n";

    std::string listData = "";

    std::string trimmedArgs = args == "LIST\r\n" ? "." : args;
    trimmedArgs.erase(trimmedArgs.find_last_not_of(" \n\r\t") + 1);

    if (!std::filesystem::exists(trimmedArgs))
        return "550 Directory not found.\r\n";

    if (!std::filesystem::is_directory(trimmedArgs))
        return "550 Cannot LIST a file.\r\n";

    pid_t pid = fork();

    if (pid < 0) {
        SocketActions::closeSocket(dataSocket);
        return "550 Failed to start directory transfer.\r\n";
    }

    if (pid == 0) {
        if (SocketActions::writeToSocket(session.clientSocket, "150 Here comes the directory listing.\r\n", 40) < 0)
            SocketActions::exitProcess(1);

        try {
            for (const auto& entry : std::filesystem::directory_iterator(trimmedArgs)) {
                std::string fileName = entry.path().filename().string() + "\r\n";
                listData += fileName;
            }
        } catch (const std::filesystem::filesystem_error&) {
            SocketActions::exitProcess(1);
        }

        if (SocketActions::writeToSocket(dataSocket, listData.c_str(), listData.size()) < 0)
            SocketActions::exitProcess(1);

        SocketActions::writeToSocket(session.clientSocket, "226 Directory send OK.\r\n", 24);
        SocketActions::closeSocket(dataSocket);
        SocketActions::exitProcess(0);
    }

    SocketActions::closeSocket(dataSocket);
    session.dataSocket = -1;

    return "";
}
