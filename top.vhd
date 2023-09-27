entity top is
	port (
			Switches : in bit_vector (9 downto 0);
			LEDR : out bit_vector (9 downto 0);
			SevenSegmentDisplay0 : out bit_vector (7 downto 0);
			SevenSegmentDisplay1 : out bit_vector (7 downto 0);
			SevenSegmentDisplay2 : out bit_vector (7 downto 0);
			SevenSegmentDisplay3 : out bit_vector (7 downto 0);
			SevenSegmentDisplay4 : out bit_vector (7 downto 0);
			SevenSegmentDisplay5 : out bit_vector (7 downto 0);
			PushButton : in bit_vector (1 downto 0);
			ADC_CLK_10 : in bit;
			MAX10_CLK1_50 : in bit;
			MAX10_CLK2_50 : in bit
			);
end entity;

entity seven_segment_decoder is
	port
	(
		ValueToDisplay: in bit_vector (3 downto 0);
		DecimalPoint: in bit;
		Display: out bit_vector (7 downto 0)
	);
end entity;

architecture decoder_arch of seven_segment_decoder is begin

   Display(7) <= not DecimalPoint;
	
	with (ValueToDisplay) select
		Display(6 downto 0) <=
			not "0111111" when "0000",
			not "0000110" when "0001",
			not "1011011" when "0010",
			not "1001111" when "0011",
			not "1100110" when "0100",
			not "1101101" when "0101",
			not "1111101" when "0110",
			not "0000111" when "0111",
			not "1111111" when "1000",
			not "1100111" when "1001",
			not "1110111" when "1010",
			not "1111100" when "1011",
			not "0111001" when "1100",
			not "1011110" when "1101",
			not "1111001" when "1110",
			not "1110001" when "1111";
end architecture;

-- 26 bit decoder.  Therefore bits 26-30

--entity T18_Timer is
--	generic(ClockFrequencyHz : integer);
--	port(
--		 Clk     : in bit;
--		 nRst    : in bit; -- Negative reset
--		 Seconds : inout integer;
--		 Minutes : inout integer;
--		 Hours   : inout integer);
--end entity;
--  
--architecture rtl of T18_Timer is
--  
--    -- Signal for counting clock periods
--    signal Ticks : integer;
--  
--begin
--  
--    process(Clk) is
--    begin
--        if rising_edge(Clk) then
--  
--            -- If the negative reset signal is active
--            if nRst = '0' then
--                Ticks   <= 0;
--                Seconds <= 0;
--                Minutes <= 0;
--                Hours   <= 0;
--            else
--  
--                -- True once every second
--                if Ticks = ClockFrequencyHz - 1 then
--                    Ticks <= 0;
--  
--                    -- True once every minute
--                    if Seconds = 59 then
--                        Seconds <= 0;
--  
--                        -- True once every hour
--                        if Minutes = 59 then
--                            Minutes <= 0;
--  
--                            -- True once a day
--                            if Hours = 23 then
--                                Hours <= 0;
--                            else
--                                Hours <= Hours + 1;
--                            end if;
--  
--                        else
--                            Minutes <= Minutes + 1;
--                        end if;
--  
--                    else
--                        Seconds <= Seconds + 1;
--                    end if;
--  
--                else
--                    Ticks <= Ticks + 1;
--                end if;
--  
--            end if;
--        end if;
--    end process;
--  
--end architecture;
--

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity UP_COUNTER is
    generic ( COUNTER_BITS : integer;
					NUM_OUTPUT_BITS : integer );
    Port ( clk: in std_logic; -- clock input
           reset: in bit; -- reset input 
           counter: out bit_vector((NUM_OUTPUT_BITS - 1) downto 0) -- output 4-bit counter
     );
end UP_COUNTER;

architecture Behavioral of UP_COUNTER is
	signal counter_up: std_logic_vector((COUNTER_BITS - 1) downto 0);
	begin
	-- up counter
	process(clk)
	begin
		if(rising_edge(clk)) then
			if(reset='1') then
				-- The 'others' syntax assigns the specified value to all bits that have NOT been assigned a value
				counter_up <= (others => '0');
			else
				counter_up <= counter_up + x"1";
			end if;
		end if;
	end process;
	counter <= to_bitvector(counter_up((COUNTER_BITS - 1) downto (COUNTER_BITS - NUM_OUTPUT_BITS)));

end Behavioral;

architecture top_arch of top is

-- 'component' declarations may occur here
	type DisplayArray is array (0 to 5) of bit_vector(7 downto 0);
	signal SegmentDisplay : DisplayArray;
	signal ValueToDisplay : bit_vector(3 downto 0);

	component seven_segment_decoder
	port
	(
		ValueToDisplay: in bit_vector (3 downto 0);
		DecimalPoint: in bit;
		Display: out bit_vector (7 downto 0)
	);
	end component seven_segment_decoder;
	
	component UP_COUNTER
    generic ( COUNTER_BITS : integer;
					NUM_OUTPUT_BITS : integer );
    Port ( clk: in bit; -- clock input
           reset: in bit; -- reset input 
           counter: out bit_vector((NUM_OUTPUT_BITS - 1) downto 0) -- output 4-bit counter
     );
	end component UP_COUNTER;
	
begin

	LEDR <= Switches;
	SevenSegmentDisplay0 <= SegmentDisplay(0);
	SevenSegmentDisplay1 <= SegmentDisplay(1);
	SevenSegmentDisplay2 <= SegmentDisplay(2);
	SevenSegmentDisplay3 <= SegmentDisplay(3);
	SevenSegmentDisplay4 <= SegmentDisplay(4);
	SevenSegmentDisplay5 <= SegmentDisplay(5);
	
	gen_converter: for N in 0 to 4 generate
		converter: seven_segment_decoder
			port map (
				Switches(((((N mod 2)+1)*4)-1) downto ((N mod 2)*4)),
							'0',
							SegmentDisplay(N) );
	end generate gen_converter;
	
	auto_clock: UP_COUNTER
		generic map( 29, 4 )
		port map
		(
			MAX10_CLK1_50,
			'0',
			ValueToDisplay
		);
		
	clock_value: seven_segment_decoder
		port map(
			ValueToDisplay,
			'0',
			SegmentDisplay(5)
			);
			
						
end architecture;

		