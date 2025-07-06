/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** CommandFactory.cpp
*/

#include "CommandFactory.hpp"

std::unordered_map<std::string, std::function<std::unique_ptr<FTPCommand>()>> CommandFactory::_factory = {
    {"USER", []() { return std::make_unique<USERCommand>(); }},
    {"PASS", []() { return std::make_unique<PASSCommand>(); }},
    {"PASS\r\n", []() { return std::make_unique<PASSCommand>(); }},
    {"QUIT\r\n", []() { return std::make_unique<QUITCommand>(); }},
    {"NOOP\r\n", []() { return std::make_unique<NOOPCommand>(); }},
    {"HELP\r\n", []() { return std::make_unique<HELPCommand>(); }},
    {"PWD\r\n", []() { return std::make_unique<PWDCommand>(); }},
    {"CDUP\r\n", []() { return std::make_unique<CDUPCommand>(); }},
    {"DELE", []() { return std::make_unique<DELECommand>(); }},
    {"CWD", []() { return std::make_unique<CWDCommand>(); }},
    {"PASV\r\n", []() { return std::make_unique<PASVCommand>(); }},
    {"PORT", []() { return std::make_unique<PORTCommand>(); }},
    {"LIST", []() { return std::make_unique<LISTCommand>(); }},
    {"LIST\r\n", []() { return std::make_unique<LISTCommand>(); }},
    {"RETR", []() { return std::make_unique<RETRCommand>(); }},
    {"STOR", []() { return std::make_unique<STORCommand>(); }}
};

std::unique_ptr<FTPCommand> CommandFactory::createCommand(const std::string& command) {
    std::string cmdType = command.substr(0, command.find(' '));

    if (_factory.find(cmdType) == _factory.end())
        return nullptr;

    return _factory[cmdType]();
}
