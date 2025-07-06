/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** USERCommand.hpp
*/

#pragma once

#include "ClientSession.hpp"
#include "FTPCommand.hpp"

class USERCommand : public FTPCommand {
public:
    std::string execute(const std::string& args, ClientSession& session) override;
};
