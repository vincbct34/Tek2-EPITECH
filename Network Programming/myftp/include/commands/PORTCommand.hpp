/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** PORTCommand.hpp
*/

#pragma once

#include <netinet/in.h>
#include <arpa/inet.h>
#include <sstream>
#include <vector>

#include "SocketActions.hpp"
#include "FTPCommand.hpp"

class PORTCommand : public FTPCommand {
public:
    std::string execute(const std::string& args, ClientSession& session) override;
};
