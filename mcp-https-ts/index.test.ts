process.env.PORT = '3000'; // Set a default port for tests

jest.mock('./logger', () => ({
  info: jest.fn(),
  error: jest.fn(),
  warn: jest.fn(),
}));

import logger from './logger';
import { app, startServer } from './index';
import request from 'supertest';
import { Server } from 'http';

let server: Server;
let exitSpy: jest.SpyInstance;

beforeAll(() => {
  exitSpy = jest.spyOn(process, 'exit').mockImplementation((code?: string | number | null | undefined) => {
    throw new Error(`process.exit called with code: ${code}`);
  });
  server = startServer();
});

afterAll(() => {
  server.close();
  exitSpy.mockRestore();
});

describe('GET /health', () => {
  it('should return a success message', async () => {
    const res = await request(app)
      .get('/health');

    expect(res.statusCode).toEqual(200);
    expect(res.body).toEqual({ message: 'Healthy' });
    expect(logger.info).toHaveBeenCalledWith("Received GET request to /health");
  });
});
