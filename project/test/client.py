import logging
import sys
import time
import asyncio

async def echo_client_Hill(event_loop):
    #using Hill port
    reader, writer = await asyncio.open_connection('127.0.0.1', 12205, loop=event_loop)
    message1 = 'IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023935.918963997\n'
    message2 = 'WHATSAT kiwi.cs.ucla.edu 10 5\n'
    try:
        print('Sending command to server: ' + message1)
        writer.write(message1.encode())
        await writer.drain()
        while not reader.at_eof():
            output = await reader.read(20000)
            print('%s' % (output.decode()))
            break

        time.sleep(1)
        print('Sending command to server: ' + message2)
        writer.write(message2.encode())
        await writer.drain()
        while not reader.at_eof():
            output = await reader.read(20000)
            print('%s' % (output.decode()))
            return
    except KeyboardInterrupt:
        print('Closing Connections')
        writer.close()
        return

event_loop = asyncio.get_event_loop()
event_loop.run_until_complete(echo_client_Hill(event_loop))
event_loop.close()
