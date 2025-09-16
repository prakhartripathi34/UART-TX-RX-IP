-- UART Receiver Module (VHDL)
-- Features: Configurable word length (5-8 bits), parity (none/odd/even/sticky), stop bits (1-2)
-- 16x oversampling for improved data recovery and noise immunity

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity uart_rx is
    generic (
        CLOCK_FREQ : integer := 50_000_000;
        BAUD_RATE  : integer := 9600;
        OVERSAMPLE : integer := 16
    );
    port (
        clk           : in  std_logic;                     -- 50 MHz system clock
        rst_n         : in  std_logic;                     -- Active low reset
        rx_serial     : in  std_logic;                     -- UART RX input
        word_len      : in  std_logic_vector(1 downto 0); -- 00=5bits, 01=6bits, 10=7bits, 11=8bits
        parity_en     : in  std_logic;                     -- Parity enable
        parity_type   : in  std_logic_vector(1 downto 0); -- 00=even, 01=odd, 10=mark(1), 11=space(0)
        stop_bits     : in  std_logic;                     -- 0=1 stop bit, 1=2 stop bits
        
        rx_data       : out std_logic_vector(7 downto 0); -- Received data
        rx_ready      : out std_logic;                     -- Data ready flag (pulse)
        parity_error  : out std_logic;                     -- Parity error flag
        frame_error   : out std_logic;                     -- Framing error flag
        overrun_error : out std_logic                      -- Overrun error flag
    );
end uart_rx;

architecture Behavioral of uart_rx is

    -- Constants
    constant BAUD_DIV : integer := CLOCK_FREQ / (BAUD_RATE * OVERSAMPLE); -- 325

    -- State definitions
    type rx_state_type is (IDLE, START_BIT, DATA_BITS, PARITY_BIT, STOP_BIT1, STOP_BIT2, CLEANUP);
    
    -- Internal signals
    signal state              : rx_state_type := IDLE;
    signal baud_counter       : integer range 0 to BAUD_DIV-1 := 0;
    signal bit_counter        : integer range 0 to 15 := 0;
    signal rx_shift_reg       : std_logic_vector(7 downto 0) := (others => '0');
    signal data_bits_to_recv  : integer range 5 to 8 := 8;
    signal expected_parity    : std_logic := '0';
    signal received_parity    : std_logic := '0';
    signal oversample_counter : integer range 0 to OVERSAMPLE-1 := 0;
    signal baud_tick          : std_logic := '0';
    signal sample_tick        : std_logic := '0';
    
    -- Synchronizer for RX input (prevent metastability)
    signal rx_sync1           : std_logic := '1';
    signal rx_sync2           : std_logic := '1';
    signal rx_sync2_d         : std_logic := '1';
    signal start_bit_edge     : std_logic := '0';

begin

    -- Input synchronization process
    process(clk, rst_n)
    begin
        if rst_n = '0' then
            rx_sync1 <= '1';
            rx_sync2 <= '1';
            rx_sync2_d <= '1';
        elsif rising_edge(clk) then
            rx_sync1 <= rx_serial;
            rx_sync2 <= rx_sync1;
            rx_sync2_d <= rx_sync2;
        end if;
    end process;

    -- Start bit edge detection (falling edge)
    start_bit_edge <= rx_sync2_d and (not rx_sync2);

    -- Determine number of data bits based on word_len
    process(word_len)
    begin
        case word_len is
            when "00"   => data_bits_to_recv <= 5;  -- 5 bits
            when "01"   => data_bits_to_recv <= 6;  -- 6 bits
            when "10"   => data_bits_to_recv <= 7;  -- 7 bits
            when "11"   => data_bits_to_recv <= 8;  -- 8 bits
            when others => data_bits_to_recv <= 8;
        end case;
    end process;

    -- Expected parity calculation
    process(parity_type, rx_shift_reg, data_bits_to_recv)
        variable parity_calc : std_logic;
    begin
        -- Calculate XOR of received data bits
        parity_calc := '0';
        for i in 0 to data_bits_to_recv-1 loop
            parity_calc := parity_calc xor rx_shift_reg(i);
        end loop;
        
        case parity_type is
            when "00"   => expected_parity <= parity_calc;      -- Even parity
            when "01"   => expected_parity <= not parity_calc;  -- Odd parity
            when "10"   => expected_parity <= '1';              -- Mark (stick 1)
            when "11"   => expected_parity <= '0';              -- Space (stick 0)
            when others => expected_parity <= parity_calc;
        end case;
    end process;

    -- Baud rate generator with 16x oversampling
    process(clk, rst_n)
    begin
        if rst_n = '0' then
            baud_counter <= 0;
            oversample_counter <= 0;
            baud_tick <= '0';
            sample_tick <= '0';
        elsif rising_edge(clk) then
            baud_tick <= '0';
            sample_tick <= '0';
            
            if baud_counter >= BAUD_DIV - 1 then
                baud_counter <= 0;
                sample_tick <= '1';  -- Sample tick every baud_div clocks
                
                if oversample_counter >= OVERSAMPLE - 1 then
                    oversample_counter <= 0;
                    baud_tick <= '1';  -- Baud tick every 16 sample ticks
                else
                    oversample_counter <= oversample_counter + 1;
                end if;
            else
                baud_counter <= baud_counter + 1;
            end if;
        end if;
    end process;

    -- Main state machine
    process(clk, rst_n)
    begin
        if rst_n = '0' then
            state <= IDLE;
            rx_data <= (others => '0');
            rx_ready <= '0';
            parity_error <= '0';
            frame_error <= '0';
            overrun_error <= '0';
            bit_counter <= 0;
            rx_shift_reg <= (others => '0');
            received_parity <= '0';
        elsif rising_edge(clk) then
            rx_ready <= '0';  -- Default: clear ready flag
            
            case state is
                when IDLE =>
                    parity_error <= '0';
                    frame_error <= '0';
                    bit_counter <= 0;
                    
                    if start_bit_edge = '1' then
                        -- Reset counters and start receiving
                        baud_counter <= 0;
                        oversample_counter <= OVERSAMPLE/2 - 1;  -- Start sampling in middle
                        state <= START_BIT;
                    end if;

                when START_BIT =>
                    if sample_tick = '1' and oversample_counter = OVERSAMPLE/2 - 1 then
                        -- Sample in the middle of start bit
                        if rx_sync2 = '0' then
                            -- Valid start bit, proceed to data
                            state <= DATA_BITS;
                            bit_counter <= 0;
                        else
                            -- False start bit, return to idle
                            state <= IDLE;
                        end if;
                    end if;

                when DATA_BITS =>
                    if sample_tick = '1' and oversample_counter = OVERSAMPLE/2 - 1 then
                        -- Sample in the middle of data bit
                        rx_shift_reg <= rx_sync2 & rx_shift_reg(7 downto 1);  -- Shift in LSB first
                        bit_counter <= bit_counter + 1;
                        
                        if bit_counter >= data_bits_to_recv - 1 then
                            if parity_en = '1' then
                                state <= PARITY_BIT;
                            else
                                state <= STOP_BIT1;
                            end if;
                        end if;
                    end if;

                when PARITY_BIT =>
                    if sample_tick = '1' and oversample_counter = OVERSAMPLE/2 - 1 then
                        -- Sample parity bit
                        received_parity <= rx_sync2;
                        state <= STOP_BIT1;
                    end if;

                when STOP_BIT1 =>
                    if sample_tick = '1' and oversample_counter = OVERSAMPLE/2 - 1 then
                        -- Sample first stop bit
                        if rx_sync2 /= '1' then
                            frame_error <= '1';  -- Stop bit should be 1
                        end if;
                        
                        if stop_bits = '1' then
                            state <= STOP_BIT2;  -- Two stop bits
                        else
                            state <= CLEANUP;    -- One stop bit
                        end if;
                    end if;

                when STOP_BIT2 =>
                    if sample_tick = '1' and oversample_counter = OVERSAMPLE/2 - 1 then
                        -- Sample second stop bit
                        if rx_sync2 /= '1' then
                            frame_error <= '1';  -- Stop bit should be 1
                        end if;
                        state <= CLEANUP;
                    end if;

                when CLEANUP =>
                    -- Check for errors and output data
                    if rx_ready = '1' then
                        overrun_error <= '1';  -- Previous data not read yet
                    end if;
                    
                    if parity_en = '1' and (received_parity /= expected_parity) then
                        parity_error <= '1';
                    end if;
                    
                    -- Align data based on word length
                    case word_len is
                        when "00"   => rx_data <= "000" & rx_shift_reg(7 downto 3);  -- 5 bits
                        when "01"   => rx_data <= "00" & rx_shift_reg(7 downto 2);   -- 6 bits
                        when "10"   => rx_data <= "0" & rx_shift_reg(7 downto 1);    -- 7 bits
                        when "11"   => rx_data <= rx_shift_reg;                      -- 8 bits
                        when others => rx_data <= rx_shift_reg;
                    end case;
                    
                    rx_ready <= '1';
                    state <= IDLE;

                when others =>
                    state <= IDLE;
                    
            end case;
        end if;
    end process;

end Behavioral;