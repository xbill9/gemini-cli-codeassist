import { greetHandler } from './index';
import logger from './logger';

// Mock the logger
jest.mock('./logger', () => ({
  info: jest.fn(),
  error: jest.fn(),
  warn: jest.fn(),
}));

describe('greetHandler', () => {
  it('should return a greeting message', async () => {
    const response = await greetHandler();
    
    expect(response).toEqual({
      content: [{ type: 'text', text: '🍎 Hello! (MCP Enabled).' }],
    });
    
    expect(logger.info).toHaveBeenCalledWith('Executed greet tool');
  });
});
