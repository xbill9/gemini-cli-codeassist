from main import greet


def test_greet_returns_param():
    """
    Test that the greet tool returns the parameter it receives.
    """
    test_param = "hello world"
    result = greet(test_param)
    assert result == test_param


def test_greet_with_empty_string():
    """
    Test that the greet tool handles an empty string parameter.
    """
    test_param = ""
    result = greet(test_param)
    assert result == test_param


def test_greet_with_special_characters():
    """
    Test that the greet tool handles parameters with special characters.
    """
    test_param = "!@#$%^&*()"
    result = greet(test_param)
    assert result == test_param
