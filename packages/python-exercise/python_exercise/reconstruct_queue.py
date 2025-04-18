def reconstruct_queue(people):
    result = []
    people.sort(key=lambda x: (-x[0], x[1]))
    for person in people:
        result.insert(person[1], person)
    return result 