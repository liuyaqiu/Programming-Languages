class A
    def m
        n
    end
    def n
        42
    end
end

class B < A
    def n
        m
    end
end

