# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # your enhancements here

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
  
  # class array holding all the pieces and their rotations
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # z
               [[[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]], # 5-length long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2], [0, -2]]],
               rotations([[0, 0], [-1, 0], [1, 0], [1, 1], [0, 1]]),
               rotations([[0, 0], [0, -1], [1, 0]])] #
end


class MyBoard < Board
  # your enhancements here
  def initialize(game)
    super    
    @current_block = MyPiece.next_piece(self)
    @cheat = false 
  end

  # rotates the current piece 180 degree
  def rotate
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

    # gets the next piece
  def next_piece
    # print("next_piece in  MyBoard\n")
    # print(@cheat)
    if @cheat 
      cheat_piece = [[[0, 0]]]
      @current_block = MyPiece.new(cheat_piece, self)
      @cheat = false 
    else
      @current_block = MyPiece.next_piece(self)

    end 
    @current_pos = nil
  end

  def cheat
    if !@cheat and @score >= 100
      @score = @score - 100
      @cheat = true
    end
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    # print("locations size= ",locations.size)
    (0..locations.size-1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
 

end

class MyTetris < Tetris
  # your enhancements here
  # def initialize
  #   print(" init for MyBoard")
  #   key_bindings
  # end
  def initialize
    super
    set_board
  end

  def key_bindings 
    super
    # print("key bindings init in MyTetris\n") 
    @root.bind('u' , proc {@board.rotate})
    @root.bind('c' , proc {@board.cheat})
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
end

