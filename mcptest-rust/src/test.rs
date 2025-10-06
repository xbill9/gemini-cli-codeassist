#[cfg(test)]
mod tests {
    use crate::HelloWorld;

    #[tokio::test]
    async fn test_hellomcp() {
        let hello_world = HelloWorld::new();
        assert_eq!(hello_world.hellomcp().await, "Hello World MCP!");
    }

    #[tokio::test]
    async fn test_rustmcp() {
        let hello_world = HelloWorld::new();
        assert_eq!(hello_world.rustmcp().await, "Hello World Rust MCP!");
    }
}