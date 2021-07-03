import requests

BASE = "http://127.0.0.1:5000/"

data = [{"likes" : 10, "name" : "haha", "views" : 30},
        {"likes" : 20, "name" : "tom", "views" : 30000},
        {"likes" : 30, "name" : "chris", "views" : 102},
        {"likes" : 40, "name" : "mok", "views" : 20000}]

for i in range(len(data)):
    #Put data  
    response = requests.put(BASE+"video/" + str(i), data[i])
    print(response.json()) 


input()

response = requests.patch(BASE+"video/2", {"views" : 99})
print(response.json())

# data = [{"likes" : 10, "name" : "haha", "views" : 30},
#         {"likes" : 20, "name" : "tom", "views" : 30000},
#         {"likes" : 30, "name" : "chris", "views" : 102},
#         {"likes" : 40, "name" : "mok", "views" : 20000}]

# for i in range(len(data)):
#     #Put data  
#     response = requests.put(BASE+"video/" + str(i), data[i])
#     print(response.json()) 

# input()

# # response = requests.delete(BASE + "video/0")   
# # print(response) #delete method doesnt return a serialized object

# # input()
response = requests.get(BASE+"video/0")
print(response.json())