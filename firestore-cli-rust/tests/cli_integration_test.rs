// tests/cli_integration_test.rs
use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::process::Command;

// This test requires the PROJECT_ID environment variable to be set.
// For example: `PROJECT_ID=your-gcp-project-id cargo test --test cli_integration_test`

#[tokio::test]
async fn test_seed_and_list() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("firestore-cli-rust")?;
    cmd.arg("seed");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Database seeded successfully."));

    let mut cmd = Command::cargo_bin("firestore-cli-rust")?;
    cmd.arg("list");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Apples"))
        .stdout(predicate::str::contains("Bananas"))
        .stdout(predicate::str::contains("Mint Chocolate Cookies"));

    Ok(())
}

#[tokio::test]
async fn test_get_product() -> Result<(), Box<dyn std::error::Error>> {
    // First, seed the database to ensure data exists
    let mut cmd = Command::cargo_bin("firestore-cli-rust")?;
    cmd.arg("seed");
    cmd.assert().success();

    // To test 'get', we need an actual ID. Seeding doesn't return IDs directly,
    // so we'll list and try to parse one, or assume a fixed product.
    // For a real integration test, you might use a known product with a predictable ID
    // or parse the list output for an ID.
    // For simplicity, let's assume "Apples" is seeded and its ID might be findable.
    // This is a simplification; in a robust test, you'd insert a product
    // specifically for the test and capture its ID.
    let mut cmd_list = Command::cargo_bin("firestore-cli-rust")?;
    cmd_list.arg("list");
    let output = cmd_list.output()?;
    let stdout = String::from_utf8(output.stdout)?;

    let apple_id_line = stdout
        .lines()
        .find(|line| line.contains("name: \"Apples\""))
        .and_then(|line| line.split("id: Some(\"").nth(1))
        .and_then(|id_part| id_part.split("\")").next());

    if let Some(id) = apple_id_line {
        let mut cmd_get = Command::cargo_bin("firestore-cli-rust")?;
        cmd_get.arg("get").arg(id);
        cmd_get
            .assert()
            .success()
            .stdout(predicate::str::contains(format!(
                "Found product: Product {{ id: Some(\"{}\"), name: \"Apples\"",
                id
            )));
    } else {
        panic!(
            "Could not find 'Apples' product ID in list output. Seed might not have worked as expected or parsing failed."
        );
    }

    Ok(())
}

#[tokio::test]
async fn test_find_product() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("firestore-cli-rust")?;
    cmd.arg("seed");
    cmd.assert().success();

    let mut cmd = Command::cargo_bin("firestore-cli-rust")?;
    cmd.arg("find").arg("milk"); // Case-insensitive, partial match
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("name: \"Milk\""));

    let mut cmd = Command::cargo_bin("firestore-cli-rust")?;
    cmd.arg("find").arg("choco"); // Partial match
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("name: \"Mint Chocolate Cookies\""));

    let mut cmd = Command::cargo_bin("firestore-cli-rust")?;
    cmd.arg("find").arg("nonexistentproduct");
    cmd.assert().success().stdout(predicate::str::contains(
        "No products found matching 'nonexistentproduct'.",
    ));

    Ok(())
}

#[tokio::test]
async fn test_delete_product() -> Result<(), Box<dyn std::error::Error>> {
    // First, seed the database to ensure data exists
    let mut cmd = Command::cargo_bin("firestore-cli-rust")?;
    cmd.arg("seed");
    cmd.assert().success();

    // Find an ID to delete
    let mut cmd_list = Command::cargo_bin("firestore-cli-rust")?;
    cmd_list.arg("list");
    let output = cmd_list.output()?;
    let stdout = String::from_utf8(output.stdout)?;

    let product_to_delete_name = "Jalapeno Seasoning"; // One of the oos products, good for deletion
    let delete_id_line = stdout
        .lines()
        .find(|line| line.contains(&format!("name: \"{}\"", product_to_delete_name)))
        .and_then(|line| line.split("id: Some(\"").nth(1))
        .and_then(|id_part| id_part.split("\")").next());

    if let Some(id_to_delete) = delete_id_line {
        // Delete the product
        let mut cmd_delete = Command::cargo_bin("firestore-cli-rust")?;
        cmd_delete.arg("delete").arg(id_to_delete);
        cmd_delete
            .assert()
            .success()
            .stdout(predicate::str::contains(format!(
                "Product with id {} deleted successfully.",
                id_to_delete
            )));

        // Verify it's gone
        let mut cmd_get = Command::cargo_bin("firestore-cli-rust")?;
        cmd_get.arg("get").arg(id_to_delete);
        cmd_get
            .assert()
            .success()
            .stdout(predicate::str::contains(format!(
                "Product with id {} not found.",
                id_to_delete
            )));
    } else {
        panic!(
            "Could not find '{}' product ID to delete.",
            product_to_delete_name
        );
    }

    Ok(())
}

#[tokio::test]
async fn test_add_product() -> Result<(), Box<dyn std::error::Error>> {
    let product_name = "New Test Product";
    let product_price = "99.99";
    let product_quantity = "10";
    let product_img = "test_image.png";

    let mut cmd = Command::cargo_bin("firestore-cli-rust")?;
    cmd.arg("add")
        .arg("--name")
        .arg(product_name)
        .arg("--price")
        .arg(product_price)
        .arg("--quantity")
        .arg(product_quantity)
        .arg("--imgfile")
        .arg(product_img);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains(format!(
            "Product '{}' added successfully.",
            product_name
        )));

    // Verify it exists via find
    let mut cmd_find = Command::cargo_bin("firestore-cli-rust")?;
    cmd_find.arg("find").arg(product_name);
    cmd_find
        .assert()
        .success()
        .stdout(predicate::str::contains(format!(
            "name: \"{}\"",
            product_name
        )))
        .stdout(predicate::str::contains(format!(
            "price: {}",
            product_price
        ))) // Note: formatting might differ (e.g., 99.99 vs 99.990)
        .stdout(predicate::str::contains(format!(
            "quantity: {}",
            product_quantity
        )));

    Ok(())
}

#[tokio::test]
async fn test_increase_decrease_stock() -> Result<(), Box<dyn std::error::Error>> {
    let product_name = "Stock Test Product";
    let product_price = "50.0";
    let initial_quantity = "100";
    let increase_amount = "20";
    let decrease_amount = "30";

    // 1. Add product
    let mut cmd_add = Command::cargo_bin("firestore-cli-rust")?;
    cmd_add
        .arg("add")
        .arg("--name")
        .arg(product_name)
        .arg("--price")
        .arg(product_price)
        .arg("--quantity")
        .arg(initial_quantity)
        .arg("--imgfile")
        .arg("stock_img.png");
    cmd_add.assert().success();

    // 2. Find ID
    let mut cmd_list = Command::cargo_bin("firestore-cli-rust")?;
    cmd_list.arg("list");
    let output = cmd_list.output()?;
    let stdout = String::from_utf8(output.stdout)?;

    let product_id = stdout
        .lines()
        .find(|line| line.contains(&format!("name: \"{}\"", product_name)))
        .and_then(|line| line.split("id: Some(\"").nth(1))
        .and_then(|id_part| id_part.split("\")").next())
        .ok_or("Could not find ID for Stock Test Product")?;

    // 3. Increase Stock
    let mut cmd_increase = Command::cargo_bin("firestore-cli-rust")?;
    cmd_increase
        .arg("increase-stock")
        .arg("--id")
        .arg(product_id)
        .arg("--amount")
        .arg(increase_amount);
    cmd_increase
        .assert()
        .success()
        .stdout(predicate::str::contains(format!(
            "Stock increased successfully for product id: {}",
            product_id
        )));

    // Verify increase (100 + 20 = 120)
    let mut cmd_get_inc = Command::cargo_bin("firestore-cli-rust")?;
    cmd_get_inc.arg("get").arg(product_id);
    cmd_get_inc
        .assert()
        .success()
        .stdout(predicate::str::contains("quantity: 120"));

    // 4. Decrease Stock
    let mut cmd_decrease = Command::cargo_bin("firestore-cli-rust")?;
    cmd_decrease
        .arg("decrease-stock")
        .arg("--id")
        .arg(product_id)
        .arg("--amount")
        .arg(decrease_amount);
    cmd_decrease
        .assert()
        .success()
        .stdout(predicate::str::contains(format!(
            "Stock decreased successfully for product id: {}",
            product_id
        )));

    // Verify decrease (120 - 30 = 90)
    let mut cmd_get_dec = Command::cargo_bin("firestore-cli-rust")?;
    cmd_get_dec.arg("get").arg(product_id);
    cmd_get_dec
        .assert()
        .success()
        .stdout(predicate::str::contains("quantity: 90"));

    Ok(())
}
