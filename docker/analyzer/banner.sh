#!/bin/bash

# This file is part of MOPSA, a Modular Open Platform for Static Analysis.
#
# Copyright (C) 2025 The MOPSA authors.
#
# SPDX-License-Identifier: LGPL-3.0-or-later
#
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option) any
# later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
# details.
#
# You should have received a copy of the GNU Lesser General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.

echo -e "\e[0;37m┏┳┓┏━┓┏━┓\e[0;97m┏━┓┏━┓\e[0m"
echo -e "\e[0;36m┃┃┃┃\e[1;37m▿\e[0;36m┃┣━┛┗━┓\e[0;96m┣━┫\e[0m"
echo -e "\e[0;94m╹ ╹┗━┛╹  ┗━┛╹ ╹\e[0m"
echo
echo -e "\e[1;35mWelcome to MOPSA!\e[0m"
echo -e -n "\e[0;90m"
mopsa -v
echo -e "\e[0m"
echo -e "Run MOPSA for instance with \e[1;33mmopsa-c <some-file.c>\e[0m"
echo -e "Source code is available at \e[1;34mhttps://gitlab.com/mopsa/mopsa-analyzer/\e[0m"
echo -e "Documentation is availble at \e[1;34mhttps://mopsa.gitlab.io/mopsa-analyzer/user-manual/\e[0m"
echo
