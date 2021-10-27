import { createLogger, format, transports } from 'winston';
import expressWinston from 'express-winston';

interface Logger {
  debug: (message: string) => void;
  info: (message: string) => void;
  warn: (message: string) => void;
  error: (message: string) => void;
}

const config = {
  format: format.combine(
    format.colorize(),
    format.json()
  ),
  transports: [
    new transports.Console({level: 'info'}),
    new transports.File({level: 'info', filename: 'access.log'})
  ]
};

export const logger: Logger = createLogger(config);
export const loggerMiddleware = expressWinston.logger({
  ...config,
  meta:true,
  expressFormat: true,
  colorize: true
})
