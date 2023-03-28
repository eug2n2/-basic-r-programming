class Node:
    def __init__(self,item):
        self.data = item
        self.link = None
        
class CircularList:
    def __init__(self):
        self.head = None
        self.tail = None
        
    def isEmpty(self): return not self.head
    
    def add_rear(self,item):
        node =Node(item)
        if not self.head:
            self.head= node
            self.tail=node
        elif self.tail:
            self.tail.link=node
            self.tail = node
        node.link = self.head
    
    def view(self):
        temp = self.head
        while temp:
            print(temp.data,end=' ')
            temp = temp.link
            if temp is self.head: break
                
    def concat(self, second):
        if not self.head:
            return second
        elif second:
            temp = self.head
            while temp.link:
                temp = temp.link
            temp.link= second
        return self.head
    
lst = CircularList()
lst2 = CircularList()
for item in [1,0,0,0,0,0,0,0]:
    lst.add_rear(item)
for item in [2,0,0,0,0,0,0,0]:
    lst2.add_rear(item)

lst.head=lst.concat(lst2.head)
lst.view()

