import request from 'supertest';
import { app, startServer } from './index';
import * as os from 'os';

let server: any;

beforeAll(async () => {
  server = await startServer();
});

afterAll(async () => {
  server.close();
});

describe('GET /health', () => {
  it('should return a 200 status and include hostname', async () => {
    const res = await request(app).get('/health');
    expect(res.statusCode).toEqual(200);
    expect(res.body.message).toEqual('Healthy');
    expect(res.body.db).toBeDefined();
    expect(res.body.hostname).toEqual(os.hostname());
  });
});
