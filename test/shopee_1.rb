# Include libs: Rspec

#Q1. Design and Implement a "Data Structure & Algorithms" for an Autocomplete System for a search engine of ecommerce website.

#1. The data structure should keep track of all "Popular Words/Sentences". (For Simplicity, This bag of words / sentences do not change dynamically).

#2. Write a function to add "Popular Words / Sentences". 
#- void AddPopularString(String something)

#3. Write a function to search and get top 3 results based on the input. (Lexicographically sorted)
#- String[] GetCompletedWord(String something)

#4. The data structure should be optimal for both space and time



#-> Data Structure - is empty
#-> AddPopularString("Airpods")
#-> AddPopularString("Air Purifier")
#-> AddPopularString("Airtag")
#-> AddPopularString("Air Cooler")
#-> AddPopularString("Airpods case")
#-> GetCompletedWord("Air") -> ["Air Cooler", "Air Purifier", "Airpods"]
#-> GetCompletedWord("Airc") -> []


# PSUEDO CODE

# AddPopularString()

# input = "Airpods"
# collections = []
# collections = ["Airpods"]
# collections = array.sort  # O(nlogn)

# GetCompletedWord()

# input = "Air"
# collections.find(str => str.includes("Air")) O(n)
# return array[0..2]


# Design a User Management System

# SCOPE

# - Authentication: Registration / Login / Logout
# - Only one type of user

# DATABASE DESIGN

# TABLE: User
# Fields:
# id => bigint
# username => string -> INDEX
# email => string(email) -> INDEX
# password => string (hashed version of the password)
# metadata => string (can be many)
# authentication_token (can be stored in redis)

# TABLE: Profile
# Fields:
# phone_number
# ALL PI
# ID CARD number
 

# FLOW DIAGRAM

USER => WEB CLIENT => LOAD BALANCER => WEB SERVER (5) => DATABASE (Partitioning)
=> REDIS (Redis Cluster) (IN MEMORY CACHE)
                                    - Auth
                                    - Caching User

## LOGIN

REQUEST LOGIN USING THE FOLLOWING

User login using
- username
- password


WE GET THE FOLLOWING

- authentication token

## INTERACT WITH THE API






# ["Air Cooler", "Air Purifier", "Airpods", "Aero", "iPhobe"]




class PopularString
    def initialize()
        @collections = []
    end
    
    def add_popular_string(value)
        @collections.push(value)
        @collections = @collections.sort
        value
    end
    
    def get_complete_word(keyword)
        populars = []
        
        @collections.each_with_index do |value, index| #O(n)
            if value[0..(keyword.length-1)] == keyword
                populars.push(value)
                return populars if populars.length == 3
            end
        end
        populars
        # return top 3 alphabetically
    end
    
    def get_complete_word_bs(keyword)
        
        # input keyword = "Air"
        
        # ["Air Cooler", "Air Purifier", "Airpods", "Bottle", "Refrigirator"]
        # length = 5
        # collection[2] => "Airpods"
        # compare "Air" <> "Airpods"
        # 
        # compare "Aerosol" <> "Airpods"
        
        # FIND THE BOUNDARY
        
        head = 0
        tail = @collections.length
        mid = ((head + tail) / 2 + 1)
        
        while(tail - head > 3)
            value = @collections[mid]
            # if mid is greater go to the left
            if @collections[mid] > keyword
                tail = mid
            end
            
            # if mid is less than equal go the right
            if @collections[mid] <= keyword
                head = mid
            end
            
            
            mid = ((head + tail) / 2 + 1)
        end
        selected_three_strings = @collections[(head)..(tail-1)]
        selected_three_strings.select { |a| a[0..(keyword.length-1)] == keyword }
    end
end


bags = PopularString.new
bags.add_popular_string("Airpods")
bags.add_popular_string("Air Purifier")
bags.add_popular_string("Airtag")
bags.add_popular_string("Air Cooler")
bags.add_popular_string("Airpods case")
bags.add_popular_string("Samsung Galaxy")
bags.add_popular_string("Samphoo")


# puts bags.get_complete_word("Air")
# puts bags.get_complete_word("Air") == ["Air Cooler", "Air Purifier", "Airpods"]

puts "TEST CASE 1"
puts bags.get_complete_word_bs("Air")
puts bags.get_complete_word_bs("Air") == ["Air Cooler", "Air Purifier", "Airpods"]


puts "TEST CASE 4"
puts bags.get_complete_word_bs("Sam")
puts bags.get_complete_word_bs("Sam") == ["Samphoo", "Samsung Galaxy"]

puts "TEST CASE 3"
puts bags.get_complete_word_bs("xyz")
puts bags.get_complete_word_bs("xyz") == []


puts "TEST CASE 1"
puts bags.get_complete_word_bs("Air Purifier")
puts bags.get_complete_word_bs("Air Purifier") == ["Air Purifier"]

puts "STOP"



