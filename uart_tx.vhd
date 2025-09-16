-- UART Transmitter Module (VHDL)
-- Features: Configurable word length (5-8 bits), parity (none/odd/even/sticky), stop bits (1-2)
-- 16x oversampling baud rate generator for 9600 baud from 50MHz clock

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity uart_tx is
    generic (
        CLOCK_FREQ : integer := 50_000_000;
        BAUD_RATE  : integer := 9600;
        OVERSAMPLE : integer := 16
    );
    port (
        clk          : in  std_logic;                     -- 50 MHz system clock
        rst_n        : in  std_logic;                     -- Active low reset
        tx_start     : in  std_logic;                     -- Start transmission trigger
        tx_data      : in  std_logic_vector(7 downto 0); -- Data to transmit (up to 8 bits)
        word_len     : in  std_logic_vector(1 downto 0); -- 00=5bits, 01=6bits, 10=7bits, 11=8bits
        parity_en    : in  std_logic;                     -- Parity enable
        parity_type  : in  std_logic_vector(1 downto 0); -- 00=even, 01=odd, 10=mark(1), 11=space(0)
        stop_bits    : in  std_logic;                     -- 0=1 stop bit, 1=2 stop bits
        
        tx_serial    : out std_logic;                     -- UART TX output
        tx_busy      : out std_logic;                     -- Transmitter busy flag
        tx_done      : out std_logic                      -- Transmission complete pulse
    );
end uart_tx;

architecture Behavioral of uart_tx is

    -- Constants
    constant BAUD_DIV : integer := CLOCK_FREQ / (BAUD_RATE * OVERSAMPLE); -- 325

    -- State definitions
    type tx_state_type is (IDLE, START_BIT, DATA_BITS, PARITY_BIT, STOP_BIT1, STOP_BIT2, DONE);
    
    -- Internal signals
    signal state            : tx_state_type := IDLE;
    signal baud_counter     : integer range 0 to BAUD_DIV-1 := 0;
    signal bit_counter      : integer range 0 to 15 := 0;
    signal tx_shift_reg     : std_logic_vector(7 downto 0) := (others => '0');
    signal parity_bit       : std_logic := '0';
    signal data_bits_to_send: integer range 5 to 8 := 8;
    signal oversample_counter: integer range 0 to OVERSAMPLE-1 := 0;
    signal baud_tick        : std_logic := '0';

begin

    -- Determine number of data bits to send based on word_len
    process(word_len)
    begin
        case word_len is
            when "00"   => data_bits_to_send <= 5;  -- 5 bits
            when "01"   => data_bits_to_send <= 6;  -- 6 bits
            when "10"   => data_bits_to_send <= 7;  -- 7 bits
            when "11"   => data_bits_to_send <= 8;  -- 8 bits
            when others => data_bits_to_send <= 8;
        end case;
    end process;

    -- Parity calculation
    process(parity_type, tx_data, data_bits_to_send)
        variable parity_calc : std_logic;
    begin
        -- Calculate XOR of data bits
        parity_calc := '0';
        for i in 0 to data_bits_to_send-1 loop
            parity_calc := parity_calc xor tx_data(i);
        end loop;
        
        case parity_type is
            when "00"   => parity_bit <= parity_calc;           -- Even parity
            when "01"   => parity_bit <= not parity_calc;       -- Odd parity
            when "10"   => parity_bit <= '1';                   -- Mark (stick 1)
            when "11"   => parity_bit <= '0';                   -- Space (stick 0)
            when others => parity_bit <= parity_calc;
        end case;
    end process;

    -- Baud rate generator with 16x oversampling
    process(clk, rst_n)
    begin
        if rst_n = '0' then
            baud_counter <= 0;
            oversample_counter <= 0;
            baud_tick <= '0';
        elsif rising_edge(clk) then
            baud_tick <= '0';
            
            if baud_counter >= BAUD_DIV - 1 then
                baud_counter <= 0;
                if oversample_counter >= OVERSAMPLE - 1 then
                    oversample_counter <= 0;
                    baud_tick <= '1';
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
            tx_serial <= '1';
            tx_busy <= '0';
            tx_done <= '0';
            bit_counter <= 0;
            tx_shift_reg <= (others => '0');
        elsif rising_edge(clk) then
            tx_done <= '0';  -- Default: clear done flag
            
            case state is
                when IDLE =>
                    tx_serial <= '1';  -- Idle high
                    tx_busy <= '0';
                    if tx_start = '1' then
                        tx_shift_reg <= tx_data;
                        bit_counter <= 0;
                        tx_busy <= '1';
                        state <= START_BIT;
                    end if;

                when START_BIT =>
                    tx_serial <= '0';  -- Start bit is always 0
                    if baud_tick = '1' then
                        state <= DATA_BITS;
                        bit_counter <= 0;
                    end if;

                when DATA_BITS =>
                    tx_serial <= tx_shift_reg(0);  -- Send LSB first
                    if baud_tick = '1' then
                        tx_shift_reg <= '0' & tx_shift_reg(7 downto 1);  -- Right shift
                        bit_counter <= bit_counter + 1;
                        if bit_counter >= data_bits_to_send - 1 then
                            if parity_en = '1' then
                                state <= PARITY_BIT;
                            else
                                state <= STOP_BIT1;
                            end if;
                        end if;
                    end if;

                when PARITY_BIT =>
                    tx_serial <= parity_bit;
                    if baud_tick = '1' then
                        state <= STOP_BIT1;
                    end if;

                when STOP_BIT1 =>
                    tx_serial <= '1';  -- Stop bit is always 1
                    if baud_tick = '1' then
                        if stop_bits = '1' then
                            state <= STOP_BIT2;  -- Two stop bits
                        else
                            state <= DONE;       -- One stop bit
                        end if;
                    end if;

                when STOP_BIT2 =>
                    tx_serial <= '1';  -- Second stop bit
                    if baud_tick = '1' then
                        state <= DONE;
                    end if;

                when DONE =>
                    tx_serial <= '1';
                    tx_busy <= '0';
                    tx_done <= '1';  -- Pulse done signal
                    state <= IDLE;

                when others =>
                    state <= IDLE;
                    
            end case;
        end if;
    end process;

end Behavioral;