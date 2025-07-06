/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Storage Module
*/

#include "../../include/StockageInfoModule.hpp"

#include <ncurses.h>
#include <sys/statvfs.h>
#include <sstream>
#include <iomanip>

StorageModule::StorageModule()
    : IModule(IModule::Mode::Performance, IModule::DataType::Text, "Storage Info")
{
    update();
}

void StorageModule::update()
{
    updateStorageInfo();
}

void StorageModule::updateStorageInfo()
{
    struct statvfs stat;
    if (statvfs("/", &stat) == 0) {
        totalStorage = stat.f_blocks * stat.f_frsize;
        freeStorage = stat.f_bfree * stat.f_frsize;
        usedStorage = totalStorage - freeStorage;
    }
}

std::string StorageModule::formatSize(long bytes) const
{
    const char* suffixes[] = {"B", "KB", "MB", "GB", "TB"};
    int i = 0;
    double size = bytes;
    while (size >= 1024 && i < 4) {
        size /= 1024;
        i++;
    }
    std::ostringstream out;
    out << std::fixed << std::setprecision(2) << size << " " << suffixes[i];
    return out.str();
}

std::string StorageModule::getData() const
{
    return "Total Storage: " + formatSize(totalStorage) + " | Used Storage: " + formatSize(usedStorage) + " | Free Storage: " + formatSize(freeStorage);
}