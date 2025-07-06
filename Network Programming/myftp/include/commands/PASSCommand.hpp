/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** PASSCommand.hpp
*/

#pragma once

#include "ClientSession.hpp"
#include "FTPCommand.hpp"

class PASSCommand : public FTPCommand {
public:
    std::string execute(const std::string& args, ClientSession& session) override;
};
