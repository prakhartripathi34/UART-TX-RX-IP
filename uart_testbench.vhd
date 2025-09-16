library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use STD.TEXTIO.ALL;
use IEEE.STD_LOGIC_TEXTIO.ALL;

entity uart_testbench is
end uart_testbench;

architecture Behavioral of uart_testbench is

    -- Parameters
    constant CLOCK_FREQ : integer := 50_000_000;
    constant BAUD_RATE  : integer := 9600;
    constant CLK_PERIOD : time := 20 ns;  -- 50MHz = 20ns period

    -- Component declarations
    component uart_tx is
        generic (
            CLOCK_FREQ : integer := 50_000_000;
            BAUD_RATE  : integer := 9600;
            OVERSAMPLE : integer := 16
        );
        port (
            clk          : in  std_logic;
            rst_n        : in  std_logic;
            tx_start     : in  std_logic;
            tx_data      : in  std_logic_vector(7 downto 0);
            word_len     : in  std_logic_vector(1 downto 0);
            parity_en    : in  std_logic;
            parity_type  : in  std_logic_vector(1 downto 0);
            stop_bits    : in  std_logic;
            tx_serial    : out std_logic;
            tx_busy      : out std_logic;
            tx_done      : out std_logic
        );
    end component;

    component uart_rx is
        generic (
            CLOCK_FREQ : integer := 50_000_000;
            BAUD_RATE  : integer := 9600;
            OVERSAMPLE : integer := 16
        );
        port (
            clk           : in  std_logic;
            rst_n         : in  std_logic;
            rx_serial     : in  std_logic;
            word_len      : in  std_logic_vector(1 downto 0);
            parity_en     : in  std_logic;
            parity_type   : in  std_logic_vector(1 downto 0);
            stop_bits     : in  std_logic;
            rx_data       : out std_logic_vector(7 downto 0);
            rx_ready      : out std_logic;
            parity_error  : out std_logic;
            frame_error   : out std_logic;
            overrun_error : out std_logic
        );
    end component;

    -- Test signals
    signal clk          : std_logic := '0';
    signal rst_n        : std_logic := '0';

    -- TX signals
    signal tx_start     : std_logic := '0';
    signal tx_data      : std_logic_vector(7 downto 0) := (others => '0');
    signal word_len     : std_logic_vector(1 downto 0) := "11";
    signal parity_en    : std_logic := '0';
    signal parity_type  : std_logic_vector(1 downto 0) := "00";
    signal stop_bits    : std_logic := '0';
    signal tx_serial    : std_logic;
    signal tx_busy      : std_logic;
    signal tx_done      : std_logic;

    -- RX signals
    signal rx_serial    : std_logic := '1';
    signal rx_data      : std_logic_vector(7 downto 0);
    signal rx_ready     : std_logic;
    signal parity_error : std_logic;
    signal frame_error  : std_logic;
    signal overrun_error: std_logic;

    -- Test data array
    type data_array_type is array (0 to 7) of std_logic_vector(7 downto 0);
    signal test_data_array : data_array_type := (
        X"AA", X"55", X"FF", X"00", X"0F", X"F0", X"3C", X"C3"
    );

    signal test_running : boolean := true;

    -- Procedure declaration should be outside any process
    procedure send_and_verify(
        data           : in std_logic_vector(7 downto 0);
        wlen           : in std_logic_vector(1 downto 0);
        parity_enable  : in std_logic;
        ptype          : in std_logic_vector(1 downto 0);
        stop_bit_count : in std_logic;
        test_name      : in string
    ) is
        variable l            : line;
        variable i            : integer := 0;
        variable expected_data: std_logic_vector(7 downto 0);
    begin
        write(l, string'("=== "));
        write(l, test_name);
        write(l, string'(" ==="));
        writeline(output, l);

        write(l, string'("Sending: 0x"));
        hwrite(l, data);
        write(l, string'(", Word_len="));
        write(l, to_integer(unsigned(wlen)) + 5);
        write(l, string'(", Parity_en="));
        write(l, parity_enable);
        write(l, string'(", Parity_type="));
        write(l, to_integer(unsigned(ptype)));
        write(l, string'(", Stop_bits="));
        write(l, to_integer(unsigned('0' & stop_bit_count)) + 1);
        writeline(output, l);

        -- Configure test parameters
        word_len <= wlen;
        parity_en <= parity_enable;
        parity_type <= ptype;
        stop_bits <= stop_bit_count;
        tx_data <= data;
        rx_serial <= tx_serial;  -- Loopback connection

        wait until rising_edge(clk);

        -- Start transmission
        tx_start <= '1';
        wait until rising_edge(clk);
        tx_start <= '0';

        -- Wait for completion and verify
        wait until tx_busy = '1';  -- Start transmission
        wait until tx_done = '1';  -- End transmission
        wait until rising_edge(clk);

        -- Allow time for RX to process
        for i in 0 to 99 loop
            wait until rising_edge(clk);
        end loop;

        -- Calculate expected data based on word length
        case wlen is
            when "00"   => expected_data := "000" & data(4 downto 0);  -- 5 bits
            when "01"   => expected_data := "00" & data(5 downto 0);   -- 6 bits
            when "10"   => expected_data := "0" & data(6 downto 0);    -- 7 bits
            when "11"   => expected_data := data;                      -- 8 bits
            when others => expected_data := data;
        end case;

        -- Check if data received correctly
        if rx_ready = '1' then
            if rx_data = expected_data and parity_error = '0' and frame_error = '0' then
                write(l, string'("✓ PASS: Received 0x"));
                hwrite(l, rx_data);
                write(l, string'(" (expected 0x"));
                hwrite(l, expected_data);
                write(l, string'(")"));
                writeline(output, l);
            else
                write(l, string'("✗ FAIL: Received 0x"));
                hwrite(l, rx_data);
                write(l, string'(" (expected 0x"));
                hwrite(l, expected_data);
                write(l, string'("), Parity_err="));
                write(l, parity_error);
                write(l, string'(", Frame_err="));
                write(l, frame_error);
                writeline(output, l);
            end if;
        else
            write(l, string'("✗ FAIL: No data received"));
            writeline(output, l);
        end if;

        writeline(output, l);  -- Empty line

        -- Gap between tests
        for i in 0 to 49 loop
            wait until rising_edge(clk);
        end loop;
    end procedure;

begin

    -- Clock generation
    clk_process: process
    begin
        while test_running loop
            clk <= '0';
            wait for CLK_PERIOD / 2;
            clk <= '1';
            wait for CLK_PERIOD / 2;
        end loop;
        wait;
    end process;

    -- Main test process
    test_process: process
        variable i : integer := 0;
        variable j : integer := 0;
        variable l : line;
    begin
        write(l, string'("UART Testbench Starting..."));
        writeline(output, l);

        -- Initialize signals
        rst_n <= '0';
        tx_start <= '0';
        tx_data <= (others => '0');
        word_len <= "11";  -- 8 bits default
        parity_en <= '0';
        parity_type <= "00";  -- Even parity
        stop_bits <= '0';     -- 1 stop bit
        rx_serial <= '1';     -- Idle high

        -- Reset sequence
        for i in 0 to 9 loop
            wait until rising_edge(clk);
        end loop;
        rst_n <= '1';
        for i in 0 to 9 loop
            wait until rising_edge(clk);
        end loop;

        -- Test 1: Basic 8-bit, no parity, 1 stop bit
        send_and_verify(X"AA", "11", '0', "00", '0', "Test 1: 8-bit, No parity, 1 stop");

        -- Test 2: 8-bit, even parity, 1 stop bit
        send_and_verify(X"55", "11", '1', "00", '0', "Test 2: 8-bit, Even parity, 1 stop");

        -- Test 3: 8-bit, odd parity, 2 stop bits
        send_and_verify(X"FF", "11", '1', "01", '1', "Test 3: 8-bit, Odd parity, 2 stop");

        -- Test 4: 7-bit, no parity, 1 stop bit
        send_and_verify(X"7F", "10", '0', "00", '0', "Test 4: 7-bit, No parity, 1 stop");

        -- Test 5: 6-bit, even parity, 1 stop bit
        send_and_verify(X"3F", "01", '1', "00", '0', "Test 5: 6-bit, Even parity, 1 stop");

        -- Test 6: 5-bit, odd parity, 2 stop bits
        send_and_verify(X"1F", "00", '1', "01", '1', "Test 6: 5-bit, Odd parity, 2 stop");

        -- Test 7: Sticky parity mark (always 1)
        send_and_verify(X"AA", "11", '1', "10", '0', "Test 7: 8-bit, Mark parity, 1 stop");

        -- Test 8: Sticky parity space (always 0)
        send_and_verify(X"55", "11", '1', "11", '0', "Test 8: 8-bit, Space parity, 1 stop");

        -- Test 9: Multiple bytes in sequence
        write(l, string'("=== Test 9: Multiple byte sequence ==="));
        writeline(output, l);

        for i in 0 to 7 loop
            word_len <= "11";  -- 8 bits
            parity_en <= '1';     -- Enable parity
            parity_type <= "00"; -- Even parity
            stop_bits <= '0';     -- 1 stop bit
            tx_data <= test_data_array(i);
            rx_serial <= tx_serial;

            tx_start <= '1';
            wait until rising_edge(clk);
            tx_start <= '0';
            -- Wait for transmission to complete inside procedure call

            -- Gap after sending one byte
            for j in 0 to 49 loop
                wait until rising_edge(clk);
            end loop;
        end loop;

        -- Test 10: Configuration matrix test
        write(l, string'("=== Test 10: Configuration matrix test ==="));
        writeline(output, l);

        for i in 0 to 3 loop      -- Word lengths: 5,6,7,8
            for j in 0 to 1 loop  -- Stop bits: 1,2
                -- Test without parity
                send_and_verify(X"A5", std_logic_vector(to_unsigned(i, 2)), '0', "00", 
                    std_logic(to_unsigned(j, 1)(0)), 
                    "Config: " & integer'image(i+5) & "-bit, no parity, " & integer'image(j+1) & " stop");

                -- Test with even parity
                send_and_verify(X"5A", std_logic_vector(to_unsigned(i, 2)), '1', "00", 
                    std_logic(to_unsigned(j, 1)(0)), 
                    "Config: " & integer'image(i+5) & "-bit, even parity, " & integer'image(j+1) & " stop");
            end loop;
        end loop;

        -- End simulation
        write(l, string'("=== All tests completed ==="));
        writeline(output, l);

        wait;
    end process;

    -- Monitor process for debugging
    monitor_process: process(clk)
        variable l : line;
    begin
        if rising_edge(clk) then
            if tx_busy = '1' or rx_ready = '1' then
                write(l, string'("Time="));
                write(l, now);
                write(l, string'(", TX_busy="));
                write(l, tx_busy);
                write(l, string'(", TX_serial="));
                write(l, tx_serial);
                write(l, string'(", RX_ready="));
                write(l, rx_ready);
                write(l, string'(", RX_data=0x"));
                hwrite(l, rx_data);
                write(l, string'(", Parity_err="));
                write(l, parity_error);
                writeline(output, l);
            end if;
        end if;
    end process;

    -- Timeout watchdog
    timeout_process: process
    begin
        wait for 50 ms;  -- 50ms timeout
        report "ERROR: Testbench timeout!" severity failure;
    end process;

end Behavioral;
