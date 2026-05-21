from main import greet, get_system_time, get_system_info


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


def test_get_system_time():
    """
    Test that get_system_time returns local and UTC time strings.
    """
    result = get_system_time()
    assert "Local Time:" in result
    assert "UTC Time:" in result


def test_get_system_info():
    """
    Test that get_system_info returns OS, CPU, Memory, and Python details.
    """
    result = get_system_info()
    assert "Operating System:" in result
    assert "Release/Kernel:" in result
    assert "Architecture:" in result
    assert "CPU Cores:" in result
    assert "Memory Info:" in result
    assert "Python Version:" in result
