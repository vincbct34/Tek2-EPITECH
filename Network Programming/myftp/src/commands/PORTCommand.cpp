/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** PORTCommand.cpp
*/

#include "ClientSession.hpp"
#include "SocketActions.hpp"
#include "PORTCommand.hpp"

std::string PORTCommand::execute(const std::string& args, ClientSession& session) {
    std::vector<int> nums;
    std::stringstream ss(args);
    std::string token;

    while (std::getline(ss, token, ',') && nums.size() < 6) {
        nums.push_back(std::stoi(token));
    }

    if (nums.size() != 6) {
        return "501 Syntax error in arguments.\r\n";
    }

    std::string ip = std::to_string(nums[0]) + "." + std::to_string(nums[1]) + "." +
                     std::to_string(nums[2]) + "." + std::to_string(nums[3]);
    int port = (nums[4] * 256) + nums[5];
    int dataSocket = SocketActions::openSocket(AF_INET, SOCK_STREAM, 0);

    if (dataSocket == -1) {
        return "425 Can't open active connection.\r\n";
    }

    sockaddr_in clientAddr{};
    clientAddr.sin_family = AF_INET;
    clientAddr.sin_port = SocketActions::convertPort(port);
    inet_pton(AF_INET, ip.c_str(), &clientAddr.sin_addr);

    if (connect(dataSocket, (struct sockaddr*)&clientAddr, sizeof(clientAddr)) == -1) {
        SocketActions::closeSocket(dataSocket);
        return "425 Can't connect to client.\r\n";
    }

    session.dataSocket = dataSocket;

    return "200 Active mode set.\r\n";
}
