using Google.Cloud.Firestore;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using ModelContextProtocol.Server;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text.Json;
using System.Threading.Tasks;

[FirestoreData]
public class Product
{
    [FirestoreDocumentId]
    public string? Id { get; set; }

    [FirestoreProperty("name")]
    public string Name { get; set; } = string.Empty;

    [FirestoreProperty("price")]
    public int Price { get; set; }

    [FirestoreProperty("quantity")]
    public int Quantity { get; set; }

    [FirestoreProperty("imgfile")]
    public string ImgFile { get; set; } = string.Empty;

    [FirestoreProperty("timestamp")]
    public DateTime Timestamp { get; set; }

    [FirestoreProperty("actualdateadded")]
    public DateTime ActualDateAdded { get; set; }
}

[McpServerToolType]
public static class MyTools
{
    private static FirestoreDb? _firestoreDb;
    private static bool _dbRunning = false;
    private static readonly Random _random = new Random();

    private static async Task<FirestoreDb?> GetFirestoreAsync()
    {
        if (_firestoreDb != null) return _firestoreDb;

        try
        {
            // Attempt to get project ID from environment
            var projectId = Environment.GetEnvironmentVariable("PROJECT_ID") ?? Environment.GetEnvironmentVariable("GOOGLE_CLOUD_PROJECT");
            
            if (string.IsNullOrEmpty(projectId))
            {
                // Fallback for local development if not set, though explicit setting is better
                projectId = "cymbal-superstore-inventory"; 
            }

            _firestoreDb = await FirestoreDb.CreateAsync(projectId);
            _dbRunning = true;
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"Failed to initialize Firestore: {ex.Message}");
            _dbRunning = false;
        }
        return _firestoreDb;
    }

    [McpServerTool]
    public static string Greet(string name)
    {
        return $"Hello, {name}!";
    }

    [McpServerTool]
    [Description("Returns the current system time.")]
    public static string GetTime()
    {
        return System.DateTime.Now.ToString();
    }

    [McpServerTool]
    [Description("Returns system specifications and information.")]
    public static string GetSystemInfo()
    {
        var os = System.Runtime.InteropServices.RuntimeInformation.OSDescription;
        var arch = System.Runtime.InteropServices.RuntimeInformation.OSArchitecture;
        var framework = System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription;
        var processorCount = System.Environment.ProcessorCount;
        var machineName = System.Environment.MachineName;

        return $"OS: {os}\nArchitecture: {arch}\nFramework: {framework}\nProcessor Count: {processorCount}\nMachine Name: {machineName}";
    }

    [McpServerTool]
    [Description("Get a list of all products from the inventory database")]
    public static async Task<string> GetProducts()
    {
        var db = await GetFirestoreAsync();
        if (!_dbRunning || db == null) return JsonSerializer.Serialize(new { error = "Inventory database is not running." });

        var snapshot = await db.Collection("inventory").GetSnapshotAsync();
        var products = snapshot.Documents.Select(doc => doc.ConvertTo<Product>()).ToList();
        return JsonSerializer.Serialize(products, new JsonSerializerOptions { WriteIndented = true });
    }

    [McpServerTool]
    [Description("Get a single product from the inventory database by its ID")]
    public static async Task<string> GetProductById([Description("The ID of the product to get")] string id)
    {
        var db = await GetFirestoreAsync();
        if (!_dbRunning || db == null) return JsonSerializer.Serialize(new { error = "Inventory database is not running." });

        var docRef = db.Collection("inventory").Document(id);
        var snapshot = await docRef.GetSnapshotAsync();

        if (!snapshot.Exists) return JsonSerializer.Serialize(new { error = "Product not found." });

        var product = snapshot.ConvertTo<Product>();
        return JsonSerializer.Serialize(product, new JsonSerializerOptions { WriteIndented = true });
    }

    [McpServerTool]
    [Description("Seed the inventory database with products.")]
    public static async Task<string> Seed()
    {
        var db = await GetFirestoreAsync();
        if (!_dbRunning || db == null) return "Inventory database is not running.";

        await InitFirestoreCollection(db);
        return "Database seeded successfully.";
    }

    [McpServerTool]
    [Description("Clears all products from the inventory database.")]
    public static async Task<string> Reset()
    {
        var db = await GetFirestoreAsync();
        if (!_dbRunning || db == null) return "Inventory database is not running.";

        await CleanFirestoreCollection(db);
        return "Database reset successfully.";
    }

    [McpServerTool]
    [Description("Checks if the inventory database is running.")]
    public static async Task<string> CheckDb()
    {
        await GetFirestoreAsync();
        return $"Database running: {_dbRunning}";
    }

    [McpServerTool]
    [Description("Get a greeting from the Cymbal Superstore Inventory API.")]
    public static string GetRoot()
    {
        return "🍎 Hello! This is the Cymbal Superstore Inventory API.";
    }

    private static async Task InitFirestoreCollection(FirestoreDb db)
    {
        string[] oldProducts = {
            "Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs", "Cheddar Cheese",
            "Whole Chicken", "Rice", "Black Beans", "Bottled Water", "Apple Juice",
            "Cola", "Coffee Beans", "Green Tea", "Watermelon", "Broccoli",
            "Jasmine Rice", "Yogurt", "Beef", "Shrimp", "Walnuts",
            "Sunflower Seeds", "Fresh Basil", "Cinnamon"
        };

        foreach (var productName in oldProducts)
        {
            var product = new Product
            {
                Name = productName,
                Price = _random.Next(1, 11),
                Quantity = _random.Next(1, 501),
                ImgFile = $"product-images/{productName.Replace(" ", "").ToLower()}.png",
                Timestamp = DateTime.UtcNow.AddMilliseconds(-_random.NextInt64(0, 31536000000) - 7776000000),
                ActualDateAdded = DateTime.UtcNow
            };
            Console.Error.WriteLine($"⬆️ Adding (or updating) product in firestore: {product.Name}");
            await AddOrUpdateFirestore(db, product);
        }

        string[] recentProducts = {
            "Parmesan Crisps", "Pineapple Kombucha", "Maple Almond Butter",
            "Mint Chocolate Cookies", "White Chocolate Caramel Corn", "Acai Smoothie Packs",
            "Smores Cereal", "Peanut Butter and Jelly Cups"
        };

        foreach (var productName in recentProducts)
        {
            var product = new Product
            {
                Name = productName,
                Price = _random.Next(1, 11),
                Quantity = _random.Next(1, 101),
                ImgFile = $"product-images/{productName.Replace(" ", "").ToLower()}.png",
                Timestamp = DateTime.UtcNow.AddMilliseconds(-_random.NextInt64(0, 518400000) + 1),
                ActualDateAdded = DateTime.UtcNow
            };
            Console.Error.WriteLine($"🆕 Adding (or updating) product in firestore: {product.Name}");
            await AddOrUpdateFirestore(db, product);
        }

        string[] recentProductsOutOfStock = { "Wasabi Party Mix", "Jalapeno Seasoning" };

        foreach (var productName in recentProductsOutOfStock)
        {
             var product = new Product
            {
                Name = productName,
                Price = _random.Next(1, 11),
                Quantity = 0,
                ImgFile = $"product-images/{productName.Replace(" ", "").ToLower()}.png",
                Timestamp = DateTime.UtcNow.AddMilliseconds(-_random.NextInt64(0, 518400000) + 1),
                ActualDateAdded = DateTime.UtcNow
            };
            Console.Error.WriteLine($"😱 Adding (or updating) out of stock product in firestore: {product.Name}");
            await AddOrUpdateFirestore(db, product);
        }
    }

    private static async Task AddOrUpdateFirestore(FirestoreDb db, Product product)
    {
        var query = db.Collection("inventory").WhereEqualTo("name", product.Name);
        var snapshot = await query.GetSnapshotAsync();

        if (snapshot.Count == 0)
        {
            await db.Collection("inventory").AddAsync(product);
        }
        else
        {
            foreach (var doc in snapshot.Documents)
            {
                // We want to update the fields but keep the ID. 
                // Using SetAsync with the product object might overwrite the ID if it's not set in the object?
                // The Product object has Id property, which is currently null for the new object.
                // If we SetAsync, we should make sure we don't mess up the ID.
                // Actually, doc.Reference.SetAsync(product) updates the document at that reference. 
                // The [FirestoreDocumentId] property in 'product' is ignored during writing if it's null?
                // Or does it overwrite the doc's ID with null? No, ID is part of the key.
                // To be safe, we can populate the ID.
                product.Id = doc.Id;
                await doc.Reference.SetAsync(product, SetOptions.MergeAll);
            }
        }
    }

    private static async Task CleanFirestoreCollection(FirestoreDb db)
    {
        Console.Error.WriteLine("Cleaning Firestore collection...");
        var snapshot = await db.Collection("inventory").GetSnapshotAsync();
        if (snapshot.Count > 0)
        {
            var batch = db.StartBatch();
            foreach (var doc in snapshot.Documents)
            {
                batch.Delete(doc.Reference);
            }
            await batch.CommitAsync();
        }
        Console.Error.WriteLine("Firestore collection cleaned.");
    }
}

public class Program
{
    public static async Task Main(string[] args)
    {
        var builder = Host.CreateApplicationBuilder(args);

        builder.Logging.AddConsole(consoleLogOptions =>
        {
            // Configure all logs to go to stderr
            consoleLogOptions.LogToStandardErrorThreshold = LogLevel.Trace;
        });

        builder.Services.AddMcpServer()
            .WithStdioServerTransport()
            .WithToolsFromAssembly();

        using IHost host = builder.Build();
        await host.RunAsync();
    }
}