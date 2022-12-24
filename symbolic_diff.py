from __future__ import annotations
from typing import Union
from abc import ABC, abstractmethod

class Expression(ABC):
    @abstractmethod
    def derive(self, var:Variable)->Expression:
        pass

    @abstractmethod
    def __add__(self, other:Expression)->Expression:
        pass

    @abstractmethod
    def __mul__(self, other:Expression)->Expression:
        pass

    @abstractmethod
    def __repr__(self):
        pass

class Variable(Expression):
    def __init__(self, name:str):
        self.name = name
    def get_name(self)->str:
        return self.name
    def derive(self, var:Variable)->Expression:
        if self.get_name != var.get_name:
            return Number(0)
        else:
            return Number(1)
    def __add__(self, other:Expression)->Expression:
        if isinstance(other, Variable) and other.get_name() == self.get_name():
            return Mul(Number(2), self)
        elif isinstance(other, Number) and other.get_value() == 0:
            return self
        else:
            return Add(self, other)
    def __mul__(self, other:Expression)->Expression:
        if isinstance(other, Number) and other.get_value() == 0:
            return Number(0)
        elif isinstance(other, Number) and other.get_value() == 1:
            return self
        return Mul(self, other)
    def __repr__(self):
        return self.get_name()

class Number(Expression):
    def __init__(self, value:Union[int, float]):
        self.value = value
    def get_value(self):
        return self.value
    def derive(self, var:Variable)->Expression:
        return Number(0)
    def __add__(self, other:Expression)->Expression:
        if isinstance(other, Number):
            return Number(self.get_value() + other.get_value())
        elif self.get_value() == 0:
            return other
        else:
            return Add(self, other)
    def __mul__(self, other:Expression)->Expression:
        if isinstance(other, Number):
            return Number(self.get_value() * other.get_value)
        elif self.get_value() == 0:
            return Number(0)
        elif self.get_value() == 1:
            return other
        else:
            return Mul(self, other)
    def __repr__(self):
        return str(self.get_value())

class Add(Expression):
    def __init__(self, augend:Expression, addend:Expression):
        self.augend = augend
        self.addend = addend
    def derive(self, var:Variable)->Expression:
        return self.augend.derive(var) + self.addend.derive(var)
    def __repr__(self):
        return f"(+ {self.augend} {self.addend})"
    def __add__(self, other:Expression)->Expression:
        if isinstance(other, Number):
            return other + self
        return Add(self, other)
    def __mul__(self, other:Expression)->Expression:
        if isinstance(other, Number):
            return other * self
        return Mul(self, other)

class Mul(Expression):
    def __init__(self, multiplicand:Expression, multiplier:Expression):
        self.multiplicand:Expression = multiplicand
        self.multiplier:Expression = multiplier
    def derive(self, var:Variable)->Expression:
        return self.multiplicand.derive(var) * self.multiplier + self.multiplicand * self.multiplier.derive(var)
    def __repr__(self):
        return f"(* {self.multiplicand} {self.multiplier})"
    def __add__(self, other:Expression)->Expression:
        if isinstance(other, Number):
            return other + self
        return Add(self, other)
    def __mul__(self, other:Expression)->Expression:
        if isinstance(other, Number):
            return other + self
        return Mul(self, other)

x = Variable("x")
one = Number(1)
y = Variable("y")
exp = x * x * x

print(exp.derive(x))