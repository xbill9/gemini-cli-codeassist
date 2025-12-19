package com.example.xbill.mcp.stdio;

import com.google.cloud.firestore.Firestore;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

@SpringBootTest
class ApplicationTests {

	@MockBean
	private Firestore firestore;

	@Test
	void contextLoads() {
	}

}
