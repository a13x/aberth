require 'bertrpc'

##
# Simple client code in ruby that calls our aberth service
##

svc = BERTRPC::Service.new('localhost', 10001)
r = svc.call.example.adder(1234, 4321)
puts r
r = svc.call.dictoid.to_dict("aberth", "is", "great!")
puts r
