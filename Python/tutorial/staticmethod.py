#Suppose you want to put all your functions into a module for organization purposes

class Math:

    @staticmethod #dont need self because it only defines functions within the class
    def add5(x):
        return x+5

print(Math.add5(10))

from inheritance import Pet
s = Pet("Joe", 30)
print(s.name)