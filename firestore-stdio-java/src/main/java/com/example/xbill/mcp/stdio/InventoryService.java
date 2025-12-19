package com.example.xbill.mcp.stdio;

import com.google.cloud.Timestamp;
import com.google.cloud.firestore.DocumentSnapshot;
import com.google.cloud.firestore.Firestore;
import com.google.cloud.firestore.QueryDocumentSnapshot;
import com.google.cloud.firestore.QuerySnapshot;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

/**
 * Service for managing inventory in Firestore.
 */
@Service
public class InventoryService {

  private static final Logger logger = LoggerFactory.getLogger(InventoryService.class);
  private static final String COLLECTION_NAME = "inventory";

  private final Firestore firestore;
  private boolean dbRunning;

  /**
   * Constructs a new InventoryService.
   *
   * @param firestore the Firestore client
   */
  public InventoryService(Firestore firestore) {
    this.firestore = firestore;
    this.dbRunning = true; // Assuming if injected, it's capable of running.
    logger.info("Firestore client initialized");
  }

  /**
   * Checks if the database is running.
   *
   * @return true if running, false otherwise
   */
  public boolean isDbRunning() {
    return dbRunning;
  }

  /**
   * Retrieves all products from Firestore.
   *
   * @return list of products
   * @throws ExecutionException if firestore operation fails
   * @throws InterruptedException if thread is interrupted
   */
  public List<Product> getAllProducts() throws ExecutionException, InterruptedException {
    var query = firestore.collection(COLLECTION_NAME).get();
    QuerySnapshot querySnapshot = query.get();
    List<Product> products = new ArrayList<>();
    for (QueryDocumentSnapshot document : querySnapshot) {
      products.add(docToProduct(document));
    }
    return products;
  }

  /**
   * Retrieves a product by its ID.
   *
   * @param id the product ID
   * @return the product, or null if not found
   * @throws ExecutionException if firestore operation fails
   * @throws InterruptedException if thread is interrupted
   */
  public Product getProductById(String id) throws ExecutionException, InterruptedException {
    var docRef = firestore.collection(COLLECTION_NAME).document(id);
    DocumentSnapshot document = docRef.get().get();
    if (document.exists()) {
      return docToProduct(document);
    }
    return null;
  }

  /**
   * Resets the inventory by deleting all documents in the collection.
   *
   * @throws ExecutionException if firestore operation fails
   * @throws InterruptedException if thread is interrupted
   */
  public void reset() throws ExecutionException, InterruptedException {
    logger.info("Cleaning Firestore collection...");
    var query = firestore.collection(COLLECTION_NAME).get();
    QuerySnapshot querySnapshot = query.get();

    if (!querySnapshot.isEmpty()) {
      var batch = firestore.batch();
      for (QueryDocumentSnapshot document : querySnapshot) {
        batch.delete(document.getReference());
      }
      batch.commit().get();
    }
    logger.info("Firestore collection cleaned.");
  }

  /**
   * Seeds the database with sample products.
   *
   * @throws ExecutionException if firestore operation fails
   * @throws InterruptedException if thread is interrupted
   */
  public void seed() throws ExecutionException, InterruptedException {
    reset(); // Clean first as per intent.

    String[] oldProducts = {
        "Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs", "Cheddar Cheese",
        "Whole Chicken", "Rice", "Black Beans", "Bottled Water", "Apple Juice",
        "Cola", "Coffee Beans", "Green Tea", "Watermelon", "Broccoli",
        "Jasmine Rice", "Yogurt", "Beef", "Shrimp", "Walnuts",
        "Sunflower Seeds", "Fresh Basil", "Cinnamon"
    };

    for (String productName : oldProducts) {
      // 31536000000 = 1 year approx in ms. 7776000000 = ~90 days in ms.
      long randomOffset = (long) (Math.random() * 31536000000L) + 7776000000L;
      Date timestamp = new Date(System.currentTimeMillis() - randomOffset);

      addOrUpdateProduct(productName,
          (int) (Math.random() * 10) + 1,
          (int) (Math.random() * 500) + 1,
          timestamp);
    }

    String[] recentProducts = {
        "Parmesan Crisps", "Pineapple Kombucha", "Maple Almond Butter",
        "Mint Chocolate Cookies", "White Chocolate Caramel Corn", "Acai Smoothie Packs",
        "Smores Cereal", "Peanut Butter and Jelly Cups"
    };

    for (String productName : recentProducts) {
      // 518400000 = ~6 days.
      long randomOffset = (long) (Math.random() * 518400000L) + 1L;
      Date timestamp = new Date(System.currentTimeMillis() - randomOffset);

      addOrUpdateProduct(productName,
          (int) (Math.random() * 10) + 1,
          (int) (Math.random() * 100) + 1,
          timestamp);
    }

    String[] recentProductsOutOfStock = {"Wasabi Party Mix", "Jalapeno Seasoning"};
    for (String productName : recentProductsOutOfStock) {
      long randomOffset = (long) (Math.random() * 518400000L) + 1L;
      Date timestamp = new Date(System.currentTimeMillis() - randomOffset);

      addOrUpdateProduct(productName,
          (int) (Math.random() * 10) + 1,
          0,
          timestamp);
    }
  }

  private void addOrUpdateProduct(String name, double price, int quantity, Date timestamp)
      throws ExecutionException, InterruptedException {

    String imgfile = "product-images/" + name.replaceAll("\\s", "").toLowerCase() + ".png";
    Date actualDateAdded = new Date();

    // Check if exists by name
    var query = firestore.collection(COLLECTION_NAME).whereEqualTo("name", name).get();

    Map<String, Object> data = new HashMap<>();
    data.put("name", name);
    data.put("price", price);
    data.put("quantity", quantity);
    data.put("imgfile", imgfile);
    data.put("timestamp", timestamp);
    data.put("actualdateadded", actualDateAdded);

    QuerySnapshot querySnapshot = query.get();
    if (querySnapshot.isEmpty()) {
      firestore.collection(COLLECTION_NAME).add(data).get();
      logger.info("Adding product in firestore: {}", name);
    } else {
      for (QueryDocumentSnapshot doc : querySnapshot) {
        firestore.collection(COLLECTION_NAME).document(doc.getId()).update(data).get();
        logger.info("Updating product in firestore: {}", name);
      }
    }
  }

  private Product docToProduct(DocumentSnapshot doc) {
    Map<String, Object> data = doc.getData();
    if (data == null) {
      throw new RuntimeException("Document data is empty");
    }
    
    // Safety checks for conversions
    String name = (String) data.get("name");
    
    // Firestore numbers can come back as Long or Double depending on value
    Number priceNum = (Number) data.get("price");
    double price = priceNum != null ? priceNum.doubleValue() : 0.0;
    
    Number quantityNum = (Number) data.get("quantity");
    int quantity = quantityNum != null ? quantityNum.intValue() : 0;
    
    String imgfile = (String) data.get("imgfile");
    
    // Timestamp handling
    Date timestamp = getDateFromObj(data.get("timestamp"));
    Date actualDateAdded = getDateFromObj(data.get("actualdateadded"));

    return new Product(doc.getId(), name, price, quantity, imgfile, timestamp, actualDateAdded);
  }

  private Date getDateFromObj(Object obj) {
    if (obj instanceof Timestamp) {
      return ((Timestamp) obj).toDate();
    } else if (obj instanceof Date) {
      return (Date) obj;
    } else if (obj instanceof String) {
      // Simple fallback, though unlikely if saved correctly
      try {
        return Date.from(java.time.Instant.parse((String) obj));
      } catch (Exception e) {
        return null;
      }
    }
    return null;
  }
}
