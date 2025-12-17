from main import get_root, check_db


def test_get_root():
    result = get_root()
    assert "Cymbal Superstore" in result


def test_check_db_initially_false():
    # Since we import main, init_firestore is NOT called automatically anymore.
    # So db_running should be False.
    result = check_db()
    assert "Database running: False" in result
