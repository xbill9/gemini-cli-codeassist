<?php

declare(strict_types=1);

use App\Tools;
use PHPUnit\Framework\TestCase;
use Psr\Log\LoggerInterface;

class ToolsTest extends TestCase
{
    private Tools $tools;

    /** @var LoggerInterface&\PHPUnit\Framework\MockObject\MockObject */
    private $logger;

    protected function setUp(): void
    {
        $this->logger = $this->createMock(LoggerInterface::class);
        $this->tools = new Tools($this->logger);
    }

    public function testGreet(): void
    {
        $this->logger->expects($this->once())
            ->method('debug')
            ->with('Executed greet tool');

        $result = $this->tools->greet('Alice');
        $this->assertSame('Hello, Alice!', $result);
    }

    public function testGetTime(): void
    {
        $this->logger->expects($this->once())
            ->method('debug')
            ->with('Executed get_time tool');

        $result = $this->tools->getTime();
        $this->assertMatchesRegularExpression('/^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}$/', $result);
    }

    public function testGetSystemSpecs(): void
    {
        $this->logger->expects($this->once())
            ->method('debug')
            ->with('Executed get_system_specs tool');

        $result = $this->tools->getSystemSpecs();
        $this->assertJson($result);
        $data = json_decode($result, true);
        $this->assertArrayHasKey('os', $data);
        $this->assertArrayHasKey('php_version', $data);
    }
}
