#!/usr/bin/env python3

from dataclasses import dataclass
from typing import Optional

OPERATOR_LEN_BITS_SAMPLE = "38006F45291200"
OPERATOR_NUM_PACKATES_SAMPLE = "EE00D40C823060"
VALUE_SAMPLE = "D2FE28"


@dataclass(frozen=True)
class ValuePacket:
    version: int
    value: int


@dataclass(frozen=True)
class OperatorPacket:
    version: int
    type: int
    packets: list["Packet"]


Packet = ValuePacket | OperatorPacket


def read_input(path: str) -> str:
    with open(path) as f:
        return f.read().strip()


def transform_to_binary(hex: str) -> str:
    result = ""

    for i in hex:
        result += "{0:04b}".format(int(i, 16))

    return result


def parse_value_packet(data: str) -> tuple[int, str]:
    result: str = ""
    rest: str = ""

    for n in range(0, len(data), 5):
        if data[n] == "0":
            result = result + data[n + 1 : n + 5]
            rest = data[n + 5 :]
            break

        result = result + data[n + 1 : n + 5]

    return int(result, 2), rest


def parse_packet(data: str) -> tuple[Packet, str]:
    packet_version = int(data[:3], 2)
    packet_type = int(data[3:6], 2)

    if packet_type == 4:
        value, rest = parse_value_packet(data[6:])
        return ValuePacket(packet_version, value), rest
    else:
        length_type_id = data[6]

        if length_type_id == "0":
            length_sub_packets = int(data[7:22], 2)
            sub_packets_data = data[22 : 22 + length_sub_packets]
            sub_packets: list[Packet] = []

            while len(sub_packets_data) > 6:
                packet, rest = parse_packet(sub_packets_data)
                sub_packets.append(packet)
                sub_packets_data = rest

            return (
                OperatorPacket(packet_version, packet_type, sub_packets),
                data[22 + length_sub_packets :],
            )
        else:
            num_sub_packets = int(data[7:18], 2)
            sub_packets_data = data[18:]
            sub_packets: list[Packet] = []

            for i in range(0, num_sub_packets):
                packet, rest = parse_packet(sub_packets_data)
                sub_packets_data = rest
                sub_packets.append(packet)

            return (
                OperatorPacket(packet_version, packet_type, sub_packets),
                sub_packets_data,
            )


def solve_part_1():
    input = read_input("inputs/day16.txt")

    packet, _ = parse_packet(transform_to_binary(input))

    queue = [packet]
    sum = 0

    while len(queue) > 0:
        p = queue.pop()
        sum += p.version

        if isinstance(p, OperatorPacket):
            queue.extend(p.packets)

        pass

    print(sum)


def sum_packets(packets: list[Packet]) -> int:
    sum = 0

    for p in packets:
        sum += evaluate(p)

    return sum


def product_packets(packets: list[Packet]) -> int:
    result = 1

    for p in packets:
        result *= evaluate(p)

    return result


def min_packets(packets: list[Packet]) -> int:
    result = evaluate(packets[0])

    for p in packets[1:]:
        value = evaluate(p)

        if value < result:
            result = value

    return result


def max_packets(packets: list[Packet]) -> int:
    result = 0

    for p in packets:
        value = evaluate(p)

        if value > result:
            result = value

    return result


def greater_than_packets(packets: list[Packet]) -> int:
    if len(packets) != 2:
        print(f"Greater than with != 2 packets! {len(packets)}")
        raise SystemExit()

    return 1 if evaluate(packets[0]) > evaluate(packets[1]) else 0


def less_than_packets(packets: list[Packet]) -> int:
    if len(packets) != 2:
        print(f"Less than with != 2 packets! {len(packets)}")
        raise SystemExit()

    return 1 if evaluate(packets[0]) < evaluate(packets[1]) else 0


def equals_to_packets(packets: list[Packet]) -> int:
    if len(packets) != 2:
        print(f"Equals with != 2 packets! {len(packets)}")
        raise SystemExit()

    return 1 if evaluate(packets[0]) == evaluate(packets[1]) else 0


def evaluate(packet: Packet) -> int:
    if isinstance(packet, ValuePacket):
        return packet.value

    operation_type = packet.type

    if operation_type == 0:
        return sum_packets(packet.packets)
    if operation_type == 1:
        return product_packets(packet.packets)
    if operation_type == 2:
        return min_packets(packet.packets)
    if operation_type == 3:
        return max_packets(packet.packets)
    if operation_type == 5:
        return greater_than_packets(packet.packets)
    if operation_type == 6:
        return less_than_packets(packet.packets)
    if operation_type == 7:
        return equals_to_packets(packet.packets)

    print(f"Unknown operation type {operation_type}")
    raise SystemExit()


def solve_part_2():
    input = read_input("inputs/day16.txt")
    # input = "9C0141080250320F1802104A08"

    packet, _ = parse_packet(transform_to_binary(input))
    print(evaluate(packet))


solve_part_1()
solve_part_2()
