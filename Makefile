# Makefile to gambit-c's sqlite3 binding.
#
# Copyright (C) 2008 Marco Benelli <mbenelli@yahoo.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

GAMBIT_ROOT=/usr/local/Gambit-C/current

GSC=$(GAMBIT_ROOT)/bin/gsc
GSI=$(GAMBIT_ROOT)/bin/gsi
GSC_OPTIONS= -f
CC_OPTIONS=""
LD_OPTIONS="-lsqlite3"
OPTIONS= $(GSC_OPTIONS) -cc-options $(CC_OPTIONS) -ld-options $(LD_OPTIONS)

.SUFFIXES: .o1 .scm
.scm.o1:
	$(GSC) $(OPTIONS) $<

sqlite3: sqlite3.o1

clean:
	rm -f *.o*
	rm -f *.c

