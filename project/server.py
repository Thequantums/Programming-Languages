import asyncio
import sys
import json
import time
import aiohttp
import async_timeout
import logging
import socket
import ssl
import re

API_KEY = 'AIzaSyCya3Brm4FSpEY8MVlxMAxpIS0810WiybM'

global_clients = {}
servers = ['Hill', 'Jaquez', 'Smith', 'Campbell', 'Singleton']
server2port_dict = {'Hill': 12205, 'Jaquez':12206, 'Smith':12207, 'Campbell':12208, 'Singleton':12209}
servers_communication_map = {'Hill': ['Jaquez', 'Smith'], 'Jaquez': ['Hill', 'Singleton'], 'Smith': ['Hill', 'Singleton', 'Campbell'], 'Campbell': ['Singleton', 'Smith'], 'Singleton':['Jaquez', 'Smith', 'Campbell']}

tasks = {}

log_file = '' #declare log file as global
server_name = ''

all_clients = {}

async def logging_file(message):
    if message == None:
        return
    try:
        log_file.write(message)
    except:
        pass

async def responsetowriter(writer, message):
    if message == None:
        return
    try:
        writer.write(message.encode())
        await writer.drain()
        writer.write_eof()
    except:
        #cannot write
        pass
        #print('Error writing message: %s' % message)
    return

#float to nearby servers
async def flood_servers(client_name):
    at_message = 'AT {} {} {} {} {} {}'.format(client_name, all_clients[client_name]['server_name'], all_clients[client_name]['latitude'], all_clients[client_name]['longitude'], str(all_clients[client_name]['time_diff']), str(all_clients[client_name]['cmd_time']))
    #this particular server has to floor to other servers from map
    for neighbor in servers_communication_map[server_name]:
        port_number = server2port_dict[neighbor]
        try:
            reader, writer = await asyncio.open_connection('127.0.0.1', port_number, loop=event_loop)
            await logging_file(server_name + ' SERVER CONNECTED TO ' + neighbor + '\n')
            await responsetowriter(writer, at_message)
            await logging_file('Success: Flooded data to %s \n' % (neighbor))
        except:
            await logging_file('Failure: writting message from %s to %s\n\n' % (client_name, neighbor))

async def process_at(writer, client_name, server_name, latitude, longitude, time_diff, ctime):
    if client_name in all_clients:
        #the client name is already seen, get the client command time
        client_time = float(all_clients[client_name]['cmd_time'])
        if client_time >= float(ctime):
            # command time is out of date, meaning the command received is REDUNDANT
            await logging_file('REDUNDANT AT COMMAND FROM: ' + client_name + '\n\n')
            return

    all_clients[client_name] = {'server_name' : server_name, 'latitude' : latitude, 'longitude' : longitude, 'time_diff' : float(time_diff), 'cmd_time' : float(ctime)}

    await flood_servers(client_name)


async def fetch_api(API_url, session):
    async with async_timeout.timeout(10):
        async with session.get(API_url) as response:
            return await response.json()

def check_latlon(latlon):
    if '+' not in latlon and '-' not in latlon:
        return False
    return True

async def process_iamat(writer, client_name, latlon, ctime, real_time):
    delay = time.time() - float(ctime)
    if (delay > 0):
        str_delay = '+' + str(delay)
    else:
        str_delay = '-' + str(delay)
    #converts latlon to latitude and longtitude
    index = 0
    count = 0
    latitude = ''
    longitude = ''
    #latlot format is wrong
    if (check_latlon(latlon) == False):
        return None
    for i in latlon:
        if i == '+' or i == '-':
            count = count + 1
            if count == 2:
                latitude = latlon[:(index -1)]
                longitude = latlon[index:]
        index = index + 1
    #add this client to list of clients
    all_clients[client_name] = {'server_name' : server_name, 'latitude' : latitude, 'longitude' : longitude, 'time_diff' : float(delay), 'cmd_time' : float(ctime)}
    message = 'AT %s %s %s %s %s' % (server_name, str_delay, client_name, latlon, ctime)
    await logging_file('RESPONDING TO IAMAT: ' + message + '\n')
    await responsetowriter(writer, message)
    await flood_servers(client_name)

async def process_whatsat(writer, command, client_name, radius, upper_bound, real_time):
    if client_name not in all_clients:
        #client id is not found
        await logging_file('Invalid client name from WHATSAT command: %s \n' % (client_name))
        await responsetowriter(writer, '? ' + command)
        return None
    
    try:
        r = float(radius)
        b = int(upper_bound)
    except ValueError:
        await logging_file('Wrong type of radius and number of results\n')
        return None

    if (r < 0 or r > 50 or b < 0 or b > 20):
        await logging_file('Radius or number of results are out of bound\n')
 
    latitude = all_clients[client_name]["latitude"]
    longitude = all_clients[client_name]["longitude"]
    latlon = latitude + longitude
    API_url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=%s,%s&radius=%d&key=%s' % (latitude, longitude, r, API_KEY)
    response = None
    async with aiohttp.ClientSession() as session:
        await logging_file('QUERYING PLACE FOR LOCATION WITH LATITUDE and LONGITUDE: (%s, %s), RADIUS: %s\n' % (latitude, longitude, radius))
        response = await fetch_api(API_url, session)
        time_delay = all_clients[client_name]["time_diff"]
        all_results = response['results'][:int(upper_bound)]
        message = 'AT %s %s %s %s %s\n%s\n\n' % (server_name, str(time_delay), client_name, latlon, real_time, json.dumps(all_results, indent=3))
        await logging_file('WHATSAT Response:\n' + message + '\n\n')
        await responsetowriter(writer, message)

async def process_command(writer, command):
    parse_command = command.split(' ')
    received_time = time.time()
    command_type = ['IAMAT','WHATSAT','AT']
    if command != '' and parse_command[0] not in command_type:
        #invalide commands
        await logging_file('Responding to invalid command: ' + '?' + command +'\n')
        whatcmd = '? ' + command
        await responsetowriter(writer, whatcmd) #write back to whoever write th command
        return None
    elif (parse_command[0] == command_type[0] or parse_command[0] == command_type[1]) and (len(parse_command) < 4):
        #invalid command
        await logging_file('Responding to invalid command:\n' + '?' + command +'\n')
        
        await responsetowriter(writer, '?' + command) #write back to whoever write th command
        return None
    elif (parse_command[0] == command_type[2] and (len(parse_command) > 7)):
        #invalid command
        await logging_file('Responding to invalid command:\n' + '?' + command +'\n')
        
        await responsetowriter(writer, '?' + command) #write back to whoever write th command
        return None        

    if (len(parse_command) > 3):
        await logging_file('RECEIVED COMMAND: ' + command + '\n')

    #Process all the three type of commands:
    if parse_command[0] == 'IAMAT':
        await process_iamat(writer, parse_command[1], parse_command[2], parse_command[3], received_time)
    if parse_command[0] == command_type[1]:
        await process_whatsat(writer, command, parse_command[1], parse_command[2], parse_command[3], received_time)
    if parse_command[0] == command_type[2]:
        await process_at(writer, parse_command[1], parse_command[2], parse_command[3], parse_command[4], parse_command[5], parse_command[6])

async def process_client(reader, writer):
    while not reader.at_eof():
        command = await reader.readline()
        await process_command(writer, command.decode())

def open_log_file(s_name):
    global log_file
    log = s_name + "_log.txt"
    open(log, "w").close() #open the files and close it to clear
    log_file = open(log, 'a+')

def check_servername(s_name):
    if s_name not in servers:
        print("Error: Server name is not found!")
        exit(1)

async def connect_client(reader, writer):
    # allow corotines (process clients) to run in asychronous
    task = asyncio.create_task(process_client(reader,writer))
    # create a list of server and client pairs
    tasks[task] =  (reader, writer)
    def close_clients(task):
        log_file.write('Close Connections.\n\n')
        del tasks[task]
        writer.close()
    task.add_done_callback(close_clients)

def main():
    if (len(sys.argv) != 2):
        print('Wrong number of arguments: need server name please!')
        exit(1)
    global server_name
    server_name = sys.argv[1]
    check_servername(server_name)
    open_log_file(server_name)

    #create event loop
    global event_loop
    log_file.write(server_name + ' starting======' + '\n')
    event_loop = asyncio.get_event_loop()
    #start servers to handle clients, use asyncio to handle many clients while process accepted clients
    serv_port = server2port_dict[server_name]
    corotine_connections = asyncio.start_server(connect_client, '127.0.0.1', serv_port, loop=event_loop)
    server = event_loop.run_until_complete(corotine_connections)

    print('Serving on {}'.format(server.sockets[0].getsockname())) #serve until ctr-c
    log_file.write('Serving on {}'.format(server.sockets[0].getsockname()) + '\n\n')

    try:
        event_loop.run_forever()
    except KeyboardInterrupt:
        pass

    server.close()
    event_loop.run_until_complete(server.wait_closed())
    event_loop.close()

if __name__ == '__main__':
    main()