class Person:
    number_of_people = 0

    def __init__(self, name, age):
        self.name = name
        self.age = age
        #Person.number_of_people += 1
        Person.add_person()

    @classmethod #Only act on class itself, never reference the instances
    def number_of_people_2(cls):
        return cls.number_of_people

    @classmethod
    def add_person(cls):
        cls.number_of_people += 1

p1 = Person("tim", 25)
#print(p1.number_of_people)
p2 = Person("John", 50)
#print(Person.number_of_people)
print(Person.number_of_people_2())