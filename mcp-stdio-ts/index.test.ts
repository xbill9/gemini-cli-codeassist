jest.mock('./logger', () => ({
  info: jest.fn(),
  error: jest.fn(),
  warn: jest.fn(),
}));

import logger from './logger';
import { greetHandler } from './index';

describe('greetHandler', () => {
  it('should return the provided parameter', async () => {
    const response = await greetHandler({ param: 'test param' });
    
    expect(response).toEqual({
      content: [{ type: 'text', text: 'test param' }],
    });
    
    expect(logger.info).toHaveBeenCalledWith('Executed greet tool');
  });
});
