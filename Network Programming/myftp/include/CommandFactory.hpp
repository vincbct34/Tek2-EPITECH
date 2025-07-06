/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** CommandFactory.hpp
*/

#pragma once

#include <unordered_map>
#include <functional>
#include <string>
#include <memory>

#include "USERCommand.hpp"
#include "PASSCommand.hpp"
#include "QUITCommand.hpp"
#include "NOOPCommand.hpp"
#include "HELPCommand.hpp"
#include "CDUPCommand.hpp"
#include "DELECommand.hpp"
#include "PASVCommand.hpp"
#include "PORTCommand.hpp"
#include "LISTCommand.hpp"
#include "RETRCommand.hpp"
#include "STORCommand.hpp"
#include "CWDCommand.hpp"
#include "PWDCommand.hpp"
#include "FTPCommand.hpp"

class CommandFactory {
    public:
        static std::unique_ptr<FTPCommand> createCommand(const std::string& command);
    private:
        static std::unordered_map<std::string, std::function<std::unique_ptr<FTPCommand>()>> _factory;
};
