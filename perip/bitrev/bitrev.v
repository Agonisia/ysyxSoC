module bitrev (
  input  sck,
  input  ss,
  input  mosi,
  output miso
);
  reg [2:0] bit_cnt;
  reg [7:0] rx_shift;
  reg [7:0] tx_shift;

  wire [7:0] rx_next = {rx_shift[6:0], mosi};
  wire [7:0] reversed_rx_next = {
    rx_next[0], rx_next[1], rx_next[2], rx_next[3],
    rx_next[4], rx_next[5], rx_next[6], rx_next[7]
  };

  always @(posedge sck or posedge ss) begin
    if (ss) begin
      bit_cnt <= 3'd0;
      rx_shift <= 8'h00;
      tx_shift <= 8'hff;
    end else begin
      rx_shift <= rx_next;
      tx_shift <= {tx_shift[6:0], 1'b1};

      if (bit_cnt == 3'd7) begin
        bit_cnt <= 3'd0;
        tx_shift <= reversed_rx_next;
      end else begin
        bit_cnt <= bit_cnt + 3'd1;
      end
    end
  end

  assign miso = ss ? 1'b1 : tx_shift[7];
endmodule
