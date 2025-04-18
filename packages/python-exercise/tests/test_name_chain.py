from python_exercise.name_chain import find_name_chain


def test_empty_input():
    assert find_name_chain([]) == []


def test_single_name():
    assert find_name_chain(["Alice"]) == ["Alice"]


def test_example_case():
    names = ["Raymond", "Nora", "Daniel", "Louie", "Peter", "Esteban"]
    expected = ["Peter", "Raymond", "Daniel", "Louie", "Esteban", "Nora"]
    assert find_name_chain(names) == expected


def test_no_valid_chain():
    names = ["Alice", "Bob", "Charlie"]  # No matching characters
    assert find_name_chain(names) == []


def test_multiple_possible_chains():
    names = ["Alice", "Eve", "Alice"]  # Multiple valid chains possible
    assert find_name_chain(names) == []


def test_case_insensitive():
    names = ["raymond", "Nora", "Daniel", "Louie", "Peter", "Esteban"]
    expected = ["Peter", "raymond", "Daniel", "Louie", "Esteban", "Nora"]
    result = find_name_chain(names)
    assert result == expected


def test_circular_chain():
    names = ["Eva", "Alice", "Eve"]
    result = find_name_chain(names)
    assert len(result) == 3
    assert result[0][-1].lower() == result[1][0].lower()
    assert result[1][-1].lower() == result[2][0].lower()
    assert result[2][-1].lower() == result[0][0].lower()


def test_duplicate_names():
    names = ["Alice", "Alice", "Eve", "Eva"]
    result = find_name_chain(names)
    expected = ['Alice', 'Eve', 'Eva', 'Alice']
    assert result == expected

def test_large_input():
    names = [
        "Alice", "Eve", "Eve", "Eve", "Eve", "Eve",
        "Eve", "Eve", "Eve", "Eve", "Eve", "Eve"
    ]
    result = find_name_chain(names)
    print(result)
    assert len(result) == len(names)
    for i in range(len(result) - 1):
        assert result[i][-1].lower() == result[i + 1][0].lower()


def test_special_characters():
    names = ["Alice!", "!Eve", "Eve@", "@Bob"]
    result = find_name_chain(names)
    assert len(result) == 4
    assert result[0][-1].lower() == result[1][0].lower()
    assert result[1][-1].lower() == result[2][0].lower()
    assert result[2][-1].lower() == result[3][0].lower()


def test_unicode_characters():
    names = ["Íris", "Yuki", "Rémy", "Müller"]
    expected = ["Müller", "Rémy", "Yuki", "Íris"]
    result = find_name_chain(names)
    assert result == expected
