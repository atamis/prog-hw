#!/usr/bin/env ruby
def t(x)
  l = (80-x.length)/2
  ";"*l + x + ";"*l
end
puts t(" " + ARGV.join(" ") + " ")
