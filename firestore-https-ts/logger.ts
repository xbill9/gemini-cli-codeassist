import winston from 'winston';

const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error', 'warn', 'info', 'debug', 'verbose', 'silly'],
      consoleWarnLevels: ['warn', 'debug', 'verbose', 'silly'] // Optional: ensures these use console.warn/error appropriately if needed, but stderrLevels forces stderr
    }),
  ],
});

export default logger;
