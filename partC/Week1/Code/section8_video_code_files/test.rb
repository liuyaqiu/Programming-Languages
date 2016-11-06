class Point
    attr_accessor :x, :y
    def initialize(x, y)
        @x = x
        @y = y
    end
end

class ColorPoint < Point
    attr_accessor :color
    def initialize(x, y, c="clear")
        super(x, y)
        @color = c
    end
    def dist(x, y)
        Math.sqrt((@x - x) ** 2 + (@y - y) ** 2)
    end
end

