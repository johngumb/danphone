#
# OpenPMR - tools to make old PMR radios useful.
#
# Copyright (C) 2013  John Gumb, G4RDC
#
# This file is part of OpenPMR.

# OpenPMR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# OpenPMR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with OpenPMR.  If not, see <http://www.gnu.org/licenses/>.

usage: McMicro using FTDI FT232R:
---------------------------------

python mui.py p

usage: DanPhone DCB4070 using FTDI FT232R USB interface
--------------------------------------------------------

python spinctrl.py p

In both cases a non-null P1 permits the PA to be enabled.

usage: McMicro using C81F350 microcontroller grafted in:
--------------------------------------------------------
python mui -6 p
This McMicro is on 6 metres.

Dependencies
------------
yum -y install wxPython
yum -y install libftdi libftdi-devel
/home/jag/pylibftdi-0.9
python setup.py install

Internals
---------
You can probably make a much better job of the UI than me.
Audio is shipped around using jack audio. This is not necessarily the best
solution but it does work.
