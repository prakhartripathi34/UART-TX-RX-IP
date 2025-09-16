@echo off
REM compile_acx.bat - Compile and run VHDL project with GHDL + GTKWave

echo === Cleaning old files ===
ghdl --clean
ghdl --remove

echo === Analyzing VHDL source files ===
ghdl -a uart_tx.vhd
ghdl -a uart_rx.vhd
ghdl -a uart_testbench.vhd

set TOP=uart_testbench

echo === Elaborating %TOP% ===
ghdl -e %TOP%

echo === Running simulation for %TOP% ===
ghdl -r %TOP% --wave=wave.ghw --stop-time=2us

echo === Opening GTKWave ===
gtkwave wave.ghw