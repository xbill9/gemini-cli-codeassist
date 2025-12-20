package com.example.xbill.mcp.https;

import java.util.Date;

/**
 * Represents a product in the inventory.
 *
 * @param id              the document ID
 * @param name            the name of the product
 * @param price           the price of the product
 * @param quantity        the quantity in stock
 * @param imgfile         the image filename
 * @param timestamp       the timestamp associated with the product
 * @param actualdateadded the actual date the product was added
 */
public record Product(
    String id,
    String name,
    double price,
    int quantity,
    String imgfile,
    Date timestamp,
    Date actualdateadded) {
}
