package com.example.mcp.server

import org.slf4j.Logger
import org.slf4j.LoggerFactory

/**
 * Extension property to get a Logger for the current class.
 * This avoids the repetitive LoggerFactory.getLogger(...) pattern.
 */
inline val <reified T> T.logger: Logger
    get() = LoggerFactory.getLogger(T::class.java)
